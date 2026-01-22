use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

pub mod utils;
use utils::*;

#[derive(Clone)]
pub struct ListenerDescriptor {
    pub once: bool,
    pub listener: Arc<dyn Fn(&Value, Option<&Value>) + Send + Sync>,
}

#[derive(Default)]
pub struct EventEmitter {
    descriptors: HashMap<String, Vec<ListenerDescriptor>>,
}

impl EventEmitter {
    pub fn new() -> Self {
        Self {
            descriptors: HashMap::new(),
        }
    }

    pub fn on<F>(&mut self, event_name: &str, listener: F)
    where
        F: Fn(&Value, Option<&Value>) + Send + Sync + 'static,
    {
        self.emit_internal("newListener", Value::String(event_name.to_string()), None);

        let list = self.descriptors.entry(event_name.to_string()).or_default();
        list.push(ListenerDescriptor {
            once: false,
            listener: Arc::new(listener),
        });
    }

    pub fn once<F>(&mut self, event_name: &str, listener: F)
    where
        F: Fn(&Value, Option<&Value>) + Send + Sync + 'static,
    {
        let list = self.descriptors.entry(event_name.to_string()).or_default();
        list.push(ListenerDescriptor {
            once: true,
            listener: Arc::new(listener),
        });
    }

    pub fn off(&mut self, event_name: &str, listener_ptr: *const ()) {
        if let Some(list) = self.descriptors.get_mut(event_name) {
            list.retain(|d| Arc::as_ptr(&d.listener) as *const () != listener_ptr);
        }
        self.emit_internal(
            "removeListener",
            Value::String(event_name.to_string()),
            Some(Value::Null),
        );
    }

    pub fn remove_all_listeners(&mut self, event_name: &str) {
        self.descriptors.insert(event_name.to_string(), vec![]);
    }

    pub fn emit(&mut self, event_name: &str, arg: &Value, arg2: Option<&Value>) -> bool {
        self.emit_internal(event_name, arg.clone(), arg2.cloned())
    }

    fn emit_internal(&mut self, event_name: &str, arg: Value, arg2: Option<Value>) -> bool {
        let Some(listeners) = self.descriptors.get_mut(event_name) else {
            return false;
        };
        if listeners.is_empty() {
            return false;
        }

        let snapshot = listeners.clone();
        let mut remove_indices = Vec::new();

        for (idx, d) in snapshot.iter().enumerate() {
            (d.listener)(&arg, arg2.as_ref());
            if d.once {
                remove_indices.push(idx);
            }
        }

        if !remove_indices.is_empty() {
            let mut kept = Vec::new();
            for (idx, d) in listeners.iter().cloned().enumerate() {
                if !remove_indices.contains(&idx) {
                    kept.push(d);
                }
            }
            *listeners = kept;
        }

        true
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ObserverInfo {
    #[serde(default)]
    pub activity: Option<String>,
    #[serde(default)]
    pub spectarget: Option<String>,
    #[serde(default)]
    pub position: Option<Vec<f64>>,
    #[serde(default)]
    pub forward: Option<Vec<f64>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BombInfo {
    pub state: String,
    #[serde(default)]
    pub countdown: Option<f64>,
    pub position: Vec<f64>,
    #[serde(default)]
    pub player: Option<ParsedPlayer>,
    #[serde(default)]
    pub site: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PhaseCountdowns {
    pub phase: String,
    pub phase_ends_in: f64,
    #[serde(default)]
    pub timeout_team: Option<ParsedTeam>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParsedRoundInfo {
    pub phase: String,
    #[serde(default)]
    pub bomb: Option<String>,
    #[serde(default)]
    pub win_team: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParsedMapInfo {
    #[serde(default)]
    pub mode: Option<String>,
    pub name: String,
    #[serde(default)]
    pub phase: Option<String>,
    #[serde(default)]
    pub round: i32,
    pub team_ct: ParsedTeam,
    pub team_t: ParsedTeam,
    #[serde(default)]
    pub num_matches_to_win_series: Option<i32>,
    #[serde(default)]
    pub current_spectators: Option<i32>,
    #[serde(default)]
    pub souvenirs_total: Option<i32>,
    #[serde(default)]
    pub round_wins: Option<HashMap<String, String>>,
    #[serde(default)]
    pub rounds: Vec<RoundWin>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CSGSIData {
    #[serde(default)]
    pub provider: Option<Value>,
    pub observer: ObserverInfo,
    #[serde(default)]
    pub round: Option<ParsedRoundInfo>,
    #[serde(default)]
    pub player: Option<ParsedPlayer>,
    pub players: Vec<ParsedPlayer>,
    #[serde(default)]
    pub bomb: Option<BombInfo>,
    pub grenades: Vec<ParsedGrenade>,
    pub phase_countdowns: PhaseCountdowns,
    pub auth: Option<Value>,
    pub map: ParsedMapInfo,
}
#[derive(Clone)]
struct TeamBuild {
    ct: ParsedTeam,
    t: ParsedTeam,
    map_name: String,
    map_round: i32,
}

#[derive(Clone)]
struct PlayersBuild {
    players: Vec<ParsedPlayer>,
    observed: Option<ParsedPlayer>,
    observer: ObserverInfo,
}

#[derive(Clone)]
struct RoundsBuild {
    rounds: Vec<RoundWin>,
    current_round_for_damage: i32,
    freeze_phase: String,
}

pub struct CSGSI {
    pub emitter: EventEmitter,
    pub teams_left: Option<TeamExtension>,
    pub teams_right: Option<TeamExtension>,
    pub players_ext: Vec<PlayerExtension>,
    pub overtime_mr: i32,
    pub regulation_mr: i32,
    pub damage: Vec<DamageRound>,
    pub last: Option<CSGSIData>,
    pub current: Option<CSGSIData>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DamagePlayer {
    pub steamid: String,
    pub damage: i32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DamageRound {
    pub round: i32,
    pub players: Vec<DamagePlayer>,
}

impl CSGSI {
    pub fn new() -> Self {
        Self {
            emitter: EventEmitter::new(),
            teams_left: None,
            teams_right: None,
            players_ext: vec![],
            overtime_mr: 3,
            regulation_mr: 15,
            damage: vec![],
            last: None,
            current: None,
        }
    }

    pub fn on<F>(&mut self, event: &str, listener: F)
    where
        F: Fn(&Value, Option<&Value>) + Send + Sync + 'static,
    {
        self.emitter.on(event, listener);
    }

    pub fn emit(&mut self, event: &str, arg: &Value, arg2: Option<&Value>) -> bool {
        self.emitter.emit(event, arg, arg2)
    }

    pub fn digest(&mut self, raw: &Value) -> Option<CSGSIData> {
        let (allplayers_obj, map, phase_countdowns) = self.validate_raw(raw)?;
        self.emit("raw", raw, None);

        let team_build = self.build_teams(map, allplayers_obj)?;
        let mut players_build = self.build_players(raw, allplayers_obj, &team_build)?;

        let rounds_build = self.build_rounds_and_damage_context(raw, map, phase_countdowns, &team_build)?;

        self.reset_damage_on_map_change(&team_build.map_name);
        self.reset_damage_on_freeze_or_warmup(team_build.map_round, &rounds_build.freeze_phase);

        self.upsert_damage_snapshot(&players_build.players, rounds_build.current_round_for_damage);
        self.apply_adr_if_possible(&mut players_build.players, team_build.map_round, rounds_build.current_round_for_damage);

        let bomb = self.build_bomb(raw, &team_build.map_name, &players_build.players);
        let grenades = self.build_grenades(raw);

        let phase = self.build_phase_countdowns(phase_countdowns, &team_build.ct, &team_build.t);
        let round_info = self.build_round_info(raw);

        let map_round_wins = map
            .get("round_wins")
            .and_then(|v| serde_json::from_value::<HashMap<String, String>>(v.clone()).ok());

        let data = CSGSIData {
            provider: raw.get("provider").cloned(),
            observer: players_build.observer,
            round: round_info,
            player: players_build.observed,
            players: players_build.players,
            bomb,
            grenades,
            phase_countdowns: phase,
            auth: raw.get("auth").cloned(),
            map: ParsedMapInfo {
                mode: map.get("mode").and_then(|v| v.as_str()).map(|s| s.to_string()),
                name: team_build.map_name.clone(),
                phase: map.get("phase").and_then(|v| v.as_str()).map(|s| s.to_string()),
                round: team_build.map_round,
                team_ct: team_build.ct.clone(),
                team_t: team_build.t.clone(),
                num_matches_to_win_series: map.get("num_matches_to_win_series").and_then(|v| v.as_i64()).map(|n| n as i32),
                current_spectators: map.get("current_spectators").and_then(|v| v.as_i64()).map(|n| n as i32),
                souvenirs_total: map.get("souvenirs_total").and_then(|v| v.as_i64()).map(|n| n as i32),
                round_wins: map_round_wins,
                rounds: rounds_build.rounds,
            },
        };

        self.current = Some(data.clone());
        self.emit_data_and_update_last(data)
    }

    fn validate_raw<'a>(
        &self,
        raw: &'a Value,
    ) -> Option<(&'a serde_json::Map<String, Value>, &'a Value, &'a Value)> {
        if raw.get("allplayers").is_none() || raw.get("map").is_none() || raw.get("phase_countdowns").is_none() {
            return None;
        }
        let allplayers_obj = raw.get("allplayers")?.as_object()?;
        let map = raw.get("map")?;
        let phase_countdowns = raw.get("phase_countdowns")?;
        Some((allplayers_obj, map, phase_countdowns))
    }

    fn build_teams(
        &self,
        map: &Value,
        allplayers_obj: &serde_json::Map<String, Value>,
    ) -> Option<TeamBuild> {
        let map_name = map.get("name")?.as_str()?.to_string();
        let map_round = map.get("round").and_then(|v| v.as_i64()).unwrap_or(0) as i32;

        let is_ct_left = self.detect_ct_left(allplayers_obj);

        let team_ct_raw: RawTeam = serde_json::from_value(map.get("team_ct")?.clone()).ok()?;
        let team_t_raw: RawTeam = serde_json::from_value(map.get("team_t")?.clone()).ok()?;

        let ct = parse_team(
            &team_ct_raw,
            if is_ct_left { "left" } else { "right" },
            "CT",
            if is_ct_left { self.teams_left.as_ref() } else { self.teams_right.as_ref() },
        );

        let t = parse_team(
            &team_t_raw,
            if is_ct_left { "right" } else { "left" },
            "T",
            if is_ct_left { self.teams_right.as_ref() } else { self.teams_left.as_ref() },
        );

        Some(TeamBuild {
            ct,
            t,
            map_name,
            map_round,
        })
    }

    fn detect_ct_left(&self, allplayers_obj: &serde_json::Map<String, Value>) -> bool {
        let mut is_ct_left = true;

        let mut example_t: Option<i32> = None;
        let mut example_ct: Option<i32> = None;

        for (_steamid, p) in allplayers_obj.iter() {
            let team = p.get("team").and_then(|v| v.as_str()).unwrap_or("");
            let slot = p.get("observer_slot").and_then(|v| v.as_i64()).map(|n| n as i32);
            if let Some(slot) = slot {
                if team == "T" && example_t.is_none() {
                    example_t = Some(slot);
                }
                if team == "CT" && example_ct.is_none() {
                    example_ct = Some(slot);
                }
            }
        }

        if let (Some(slot_ct), Some(slot_t)) = (example_ct, example_t) {
            if slot_ct > slot_t {
                is_ct_left = false;
            }
        }

        is_ct_left
    }

    fn build_players(
        &self,
        raw: &Value,
        allplayers_obj: &serde_json::Map<String, Value>,
        team_build: &TeamBuild,
    ) -> Option<PlayersBuild> {
        let mut base_players: HashMap<String, BasePlayer> = HashMap::new();
        for (steamid, p) in allplayers_obj.iter() {
            if let Ok(bp) = serde_json::from_value::<BasePlayer>(p.clone()) {
                base_players.insert(steamid.clone(), bp);
            }
        }

        let mut parsed_players = Vec::new();
        for (steamid, bp) in base_players.iter() {
            let team = if bp.team == "CT" { team_build.ct.clone() } else { team_build.t.clone() };
            parsed_players.push(parse_player(bp, steamid, team, &self.players_ext));
        }

        let observed = raw
            .get("player")
            .and_then(|p| p.get("steamid"))
            .and_then(|v| v.as_str())
            .and_then(|sid| parsed_players.iter().find(|pl| pl.steamid == sid).cloned());

        let observer = ObserverInfo {
            activity: raw
                .get("player")
                .and_then(|p| p.get("activity"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            spectarget: raw
                .get("player")
                .and_then(|p| p.get("spectarget"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            position: parse_vec3_from_opt_str(raw.get("player").and_then(|p| p.get("position"))),
            forward: parse_vec3_from_opt_str(raw.get("player").and_then(|p| p.get("forward"))),
        };

        Some(PlayersBuild {
            players: parsed_players,
            observed,
            observer,
        })
    }

    fn build_rounds_and_damage_context(
        &self,
        raw: &Value,
        map: &Value,
        phase_countdowns: &Value,
        team_build: &TeamBuild,
    ) -> Option<RoundsBuild> {
        let map_round = team_build.map_round;

        let rounds = self.compute_round_wins(raw, map, &team_build.ct, &team_build.t, map_round);
        let current_round_for_damage = self.current_round_index(raw, map, map_round);
        let freeze_phase = phase_countdowns
            .get("phase")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        Some(RoundsBuild {
            rounds,
            current_round_for_damage,
            freeze_phase,
        })
    }

    fn is_round_finalized(&self, raw: &Value, map: &Value) -> bool {
        let phase = raw
            .get("round")
            .and_then(|r| r.get("phase"))
            .and_then(|v| v.as_str())
            .unwrap_or("");

        let map_phase = map
            .get("phase")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        phase == "over" || map_phase == "gameover"
    }

    fn current_round_index(&self, raw: &Value, map: &Value, map_round: i32) -> i32 {
        if self.is_round_finalized(raw, map) {
            map_round
        } else {
            map_round + 1
        }
    }

    fn compute_round_wins(
        &self,
        raw: &Value,
        map: &Value,
        team_ct: &ParsedTeam,
        team_t: &ParsedTeam,
        map_round: i32,
    ) -> Vec<RoundWin> {
        let Some(round_wins) = map.get("round_wins").and_then(|v| v.as_object()) else {
            return vec![];
        };

        let rw: HashMap<String, String> = round_wins
            .iter()
            .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
            .collect();

        let current_round = self.current_round_index(raw, map, map_round);

        let mut rounds = Vec::new();
        for i in 1..=current_round {
            if let Some(res) = get_round_win(
                current_round,
                team_ct,
                team_t,
                &rw,
                i,
                self.regulation_mr,
                self.overtime_mr,
            ) {
                rounds.push(res);
            }
        }

        rounds
    }

    fn reset_damage_on_map_change(&mut self, map_name: &str) {
        if let Some(last) = &self.last {
            if last.map.name != map_name {
                self.damage.clear();
            }
        }
    }

    fn reset_damage_on_freeze_or_warmup(&mut self, map_round: i32, phase: &str) {
        if (map_round == 0 && phase == "freezetime") || phase == "warmup" {
            self.damage.clear();
        }
    }

    fn upsert_damage_snapshot(&mut self, players: &[ParsedPlayer], round: i32) {
        if !self.damage.iter().any(|d| d.round == round) {
            self.damage.push(DamageRound {
                round,
                players: vec![],
            });
        }

        if let Some(dmg_round) = self.damage.iter_mut().find(|d| d.round == round) {
            dmg_round.players = players
                .iter()
                .map(|p| DamagePlayer {
                    steamid: p.steamid.clone(),
                    damage: p.state.round_totaldmg,
                })
                .collect();
        }
    }

    fn apply_adr_if_possible(&self, players: &mut [ParsedPlayer], map_round: i32, current_round_for_damage: i32) {
        if self.current.is_none() {
            return;
        }

        let damage_for_round = self
            .damage
            .iter()
            .filter(|d| d.round < current_round_for_damage)
            .collect::<Vec<_>>();

        if damage_for_round.is_empty() {
            return;
        }

        let denom = if map_round == 0 { 1 } else { map_round } as f64;

        for pl in players.iter_mut() {
            let mut sum = 0i32;
            for dr in damage_for_round.iter() {
                let v = dr
                    .players
                    .iter()
                    .find(|x| x.steamid == pl.steamid)
                    .map(|x| x.damage)
                    .unwrap_or(0);
                sum += v;
            }
            let adr = (sum as f64) / denom;
            pl.state.adr = adr.floor() as i32;
        }
    }

    fn build_bomb(&self, raw: &Value, map_name: &str, players: &[ParsedPlayer]) -> Option<BombInfo> {
        let bomb = raw.get("bomb")?;
        let state = bomb.get("state").and_then(|v| v.as_str()).unwrap_or("").to_string();

        let position = bomb
            .get("position")
            .and_then(|v| v.as_str())
            .map(|s| {
                s.split(", ")
                    .map(|p| p.parse::<f64>().unwrap_or(0.0))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(|| vec![0.0, 0.0, 0.0]);

        let countdown = parse_f64_from_str(bomb.get("countdown"));

        let bomb_player = bomb
            .get("player")
            .and_then(|v| v.as_str())
            .and_then(|sid| players.iter().find(|p| p.steamid == sid).cloned());

        let site = if state == "planted" || state == "defused" || state == "defusing" || state == "planting" {
            Self::find_site(map_name, &position)
        } else {
            None
        };

        Some(BombInfo {
            state,
            countdown,
            position,
            player: bomb_player,
            site,
        })
    }

    fn build_grenades(&self, raw: &Value) -> Vec<ParsedGrenade> {
        parse_grenades(raw.get("grenades").or_else(|| raw.get("grenades")))
    }

    fn build_phase_countdowns(&self, phase_countdowns: &Value, team_ct: &ParsedTeam, team_t: &ParsedTeam) -> PhaseCountdowns {
        let phase = phase_countdowns
            .get("phase")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let phase_ends_in = phase_countdowns
            .get("phase_ends_in")
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);

        let timeout_team = if phase == "timeout_ct" {
            Some(team_ct.clone())
        } else if phase == "timeout_t" {
            Some(team_t.clone())
        } else {
            None
        };

        PhaseCountdowns {
            phase,
            phase_ends_in,
            timeout_team,
        }
    }

    fn build_round_info(&self, raw: &Value) -> Option<ParsedRoundInfo> {
        raw.get("round").and_then(|r| {
            Some(ParsedRoundInfo {
                phase: r.get("phase")?.as_str()?.to_string(),
                bomb: r.get("bomb").and_then(|v| v.as_str()).map(|s| s.to_string()),
                win_team: r.get("win_team").and_then(|v| v.as_str()).map(|s| s.to_string()),
            })
        })
    }

    fn emit_data_and_update_last(&mut self, data: CSGSIData) -> Option<CSGSIData> {
        if self.last.is_none() {
            self.last = Some(data.clone());
            let v = serde_json::to_value(&data).ok().unwrap_or(Value::Null);
            self.emit("data", &v, None);
            return Some(data);
        }

        self.last = Some(data.clone());
        let v = serde_json::to_value(&data).ok().unwrap_or(Value::Null);
        self.emit("data", &v, None);
        Some(data)
    }

    pub fn find_site(map_name: &str, position: &[f64]) -> Option<String> {
        let real = map_name.rsplit('/').next().unwrap_or(map_name);
        let site = match real {
            "de_mirage" => if position.get(1).copied().unwrap_or(0.0) < -600.0 { "A" } else { "B" },
            "de_cache" => if position.get(1).copied().unwrap_or(0.0) > 0.0 { "A" } else { "B" },
            "de_overpass" => if position.get(2).copied().unwrap_or(0.0) > 400.0 { "A" } else { "B" },
            "de_nuke" => if position.get(2).copied().unwrap_or(0.0) > -500.0 { "A" } else { "B" },
            "de_dust2" => if position.get(0).copied().unwrap_or(0.0) > -500.0 { "A" } else { "B" },
            "de_inferno" => if position.get(0).copied().unwrap_or(0.0) > 1400.0 { "A" } else { "B" },
            "de_vertigo" => if position.get(0).copied().unwrap_or(0.0) > -1400.0 { "A" } else { "B" },
            "de_train" => if position.get(1).copied().unwrap_or(0.0) > -450.0 { "A" } else { "B" },
            "de_ancient" => if position.get(0).copied().unwrap_or(0.0) < -500.0 { "A" } else { "B" },
            "de_anubis" => if position.get(0).copied().unwrap_or(0.0) > 0.0 { "A" } else { "B" },
            _ => return None,
        };
        Some(site.to_string())
    }
}
