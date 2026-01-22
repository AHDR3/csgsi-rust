use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PlayerExtension {
    pub steamid: String,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub avatar: Option<String>,
    #[serde(default)]
    pub country: Option<String>,
    #[serde(default)]
    pub real_name: Option<String>,
    #[serde(default)]
    pub extra: Option<Value>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TeamExtension {
    #[serde(default)]
    pub logo: Option<String>,
    #[serde(default)]
    pub map_score: Option<i32>,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub country: Option<String>,
    #[serde(default)]
    pub id: Option<String>,
    #[serde(default)]
    pub extra: Option<Value>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[derive(Default)]
pub struct MatchStats {
    #[serde(default)]
    pub mvps: i32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PlayerState {
    #[serde(default)]
    pub smoked: i32,
    #[serde(default)]
    pub round_totaldmg: i32,
    #[serde(default)]
    pub adr: i32,
    #[serde(flatten)]
    pub rest: HashMap<String, Value>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BasePlayer {
    pub name: String,
    #[serde(default)]
    pub clan: Option<String>,
    #[serde(default)]
    pub observer_slot: Option<i32>,
    #[serde(default)]
    pub match_stats: MatchStats,
    #[serde(default)]
    pub weapons: HashMap<String, Value>,
    pub state: PlayerState,
    pub position: String,
    pub forward: String,
    pub team: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WeaponWithId {
    pub id: String,
    #[serde(flatten)]
    pub data: Value,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParsedPlayer {
    pub steamid: String,
    pub name: String,
    pub default_name: String,
    #[serde(default)]
    pub clan: Option<String>,
    #[serde(default)]
    pub observer_slot: Option<i32>,
    pub stats: MatchStats,
    pub weapons: Vec<WeaponWithId>,
    pub state: PlayerState,
    pub position: Vec<f64>,
    pub forward: Vec<f64>,
    pub team: ParsedTeam,
    pub avatar: Option<String>,
    pub country: Option<String>,
    pub real_name: Option<String>,
    pub extra: Value,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RawTeam {
    pub score: i32,
    #[serde(default)]
    pub consecutive_round_losses: i32,
    #[serde(default)]
    pub timeouts_remaining: i32,
    #[serde(default)]
    pub matches_won_this_series: i32,
    #[serde(default)]
    pub name: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParsedTeam {
    pub score: i32,
    pub logo: Option<String>,
    pub consecutive_round_losses: i32,
    pub timeouts_remaining: i32,
    pub matches_won_this_series: i32,
    pub side: String,
    pub name: String,
    pub country: Option<String>,
    pub id: Option<String>,
    pub orientation: String,
    pub extra: Value,
}

fn parse_vec3(s: &str) -> Vec<f64> {
    s.split(", ")
        .map(|p| p.parse::<f64>().unwrap_or(0.0))
        .collect()
}

pub fn parse_player(
    base_player: &BasePlayer,
    steamid: &str,
    team: ParsedTeam,
    extensions: &[PlayerExtension],
) -> ParsedPlayer {
    let ext = extensions.iter().find(|p| p.steamid == steamid);

    let mut state = base_player.state.clone();
    if state.smoked == 0 {
        state.smoked = 0;
    }
    state.adr = 0;

    let weapons = base_player
        .weapons
        .iter()
        .map(|(id, weapon)| WeaponWithId {
            id: id.clone(),
            data: weapon.clone(),
        })
        .collect::<Vec<_>>();

    ParsedPlayer {
        steamid: steamid.to_string(),
        name: ext.and_then(|e| e.name.clone()).unwrap_or_else(|| base_player.name.clone()),
        default_name: base_player.name.clone(),
        clan: base_player.clan.clone(),
        observer_slot: base_player.observer_slot,
        stats: base_player.match_stats.clone(),
        weapons,
        state,
        position: parse_vec3(&base_player.position),
        forward: parse_vec3(&base_player.forward),
        team,
        avatar: ext.and_then(|e| e.avatar.clone()),
        country: ext.and_then(|e| e.country.clone()),
        real_name: ext.and_then(|e| e.real_name.clone()),
        extra: ext
            .and_then(|e| e.extra.clone())
            .unwrap_or_else(|| Value::Object(Default::default())),
    }
}

pub fn parse_team(
    team: &RawTeam,
    orientation: &str,
    side: &str,
    ext: Option<&TeamExtension>,
) -> ParsedTeam {
    let default_name = if side == "CT" {
        "Counter-Terrorists"
    } else {
        "Terrorists"
    };

    let name = ext
        .and_then(|e| e.name.clone())
        .or_else(|| team.name.clone())
        .unwrap_or_else(|| default_name.to_string());

    let matches_won = ext
        .and_then(|e| e.map_score)
        .unwrap_or(team.matches_won_this_series);

    ParsedTeam {
        score: team.score,
        logo: ext.and_then(|e| e.logo.clone()),
        consecutive_round_losses: team.consecutive_round_losses,
        timeouts_remaining: team.timeouts_remaining,
        matches_won_this_series: matches_won,
        side: side.to_string(),
        name,
        country: ext.and_then(|e| e.country.clone()),
        id: ext.and_then(|e| e.id.clone()),
        orientation: orientation.to_string(),
        extra: ext
            .and_then(|e| e.extra.clone())
            .unwrap_or_else(|| Value::Object(Default::default())),
    }
}

pub fn get_half_from_round(round: i32, regulation_mr: i32, mr: i32) -> i32 {
    let current_half;
    if round <= 2 * regulation_mr {
        current_half = if round <= regulation_mr { 1 } else { 2 };
    } else {
        let round_in_ot = (round - (2 * regulation_mr + 1)).rem_euclid(mr * 2) + 1;
        current_half = if round_in_ot <= mr { 1 } else { 2 };
    }
    current_half
}

pub fn did_team_win_that_round(
    team: &ParsedTeam,
    round: i32,
    won_by: &str,
    current_round: i32,
    regulation_mr: i32,
    mr: i32,
) -> bool {
    let current_half = get_half_from_round(current_round, regulation_mr, mr);
    let check_half = get_half_from_round(round, regulation_mr, mr);

    let a = team.side == won_by;
    let b = current_half == check_half;
    a == b
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InfernoFlame {
    pub id: String,
    pub position: Vec<f64>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParsedGrenade {
    pub id: String,
    #[serde(rename = "type")]
    pub kind: String,
    #[serde(default)]
    pub owner: Option<String>,
    #[serde(default)]
    pub velocity: Option<Vec<f64>>,
    #[serde(default)]
    pub position: Option<Vec<f64>>,
    #[serde(default)]
    pub lifetime: Option<f64>,
    #[serde(default)]
    pub effecttime: Option<f64>,
    #[serde(default)]
    pub flames: Option<Vec<InfernoFlame>>,
    #[serde(flatten)]
    pub rest: HashMap<String, Value>,
}

fn split_floats(s: &str) -> Vec<f64> {
    s.split(", ")
        .map(|p| p.parse::<f64>().unwrap_or(0.0))
        .collect()
}

pub fn parse_grenade(grenade: &Value, id: &str) -> Option<ParsedGrenade> {
    let kind = grenade.get("type")?.as_str()?.to_string();

    if kind == "inferno" {
        let mut flames_out = Vec::new();
        if let Some(flames) = grenade.get("flames").and_then(|v| v.as_object()) {
            for (fid, pos) in flames {
                let pos_str = pos.as_str().unwrap_or("");
                flames_out.push(InfernoFlame {
                    id: fid.clone(),
                    position: split_floats(pos_str),
                });
            }
        }

        let lifetime = grenade
            .get("lifetime")
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f64>().ok());

        let mut rest = HashMap::new();
        if let Some(obj) = grenade.as_object() {
            for (k, v) in obj {
                if k != "flames" && k != "lifetime" && k != "type" {
                    rest.insert(k.clone(), v.clone());
                }
            }
        }

        return Some(ParsedGrenade {
            id: id.to_string(),
            kind,
            owner: grenade.get("owner").and_then(|v| v.as_str()).map(|s| s.to_string()),
            velocity: None,
            position: None,
            lifetime,
            effecttime: None,
            flames: Some(flames_out),
            rest,
        });
    }

    if kind == "smoke" || kind == "decoy" {
        let velocity = grenade
            .get("velocity")
            .and_then(|v| v.as_str())
            .map(split_floats);

        let position = grenade
            .get("position")
            .and_then(|v| v.as_str())
            .map(split_floats);

        let lifetime = grenade
            .get("lifetime")
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f64>().ok());

        let effecttime = grenade
            .get("effecttime")
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f64>().ok());

        let mut rest = HashMap::new();
        if let Some(obj) = grenade.as_object() {
            for (k, v) in obj {
                if k != "velocity" && k != "position" && k != "lifetime" && k != "effecttime" && k != "type" {
                    rest.insert(k.clone(), v.clone());
                }
            }
        }

        return Some(ParsedGrenade {
            id: id.to_string(),
            kind,
            owner: grenade.get("owner").and_then(|v| v.as_str()).map(|s| s.to_string()),
            velocity,
            position,
            lifetime,
            effecttime,
            flames: None,
            rest,
        });
    }

    let velocity = grenade
        .get("velocity")
        .and_then(|v| v.as_str())
        .map(split_floats);

    let position = grenade
        .get("position")
        .and_then(|v| v.as_str())
        .map(split_floats);

    let lifetime = grenade
        .get("lifetime")
        .and_then(|v| v.as_str())
        .and_then(|s| s.parse::<f64>().ok());

    let mut rest = HashMap::new();
    if let Some(obj) = grenade.as_object() {
        for (k, v) in obj {
            if k != "velocity" && k != "position" && k != "lifetime" && k != "type" && k != "owner" {
                rest.insert(k.clone(), v.clone());
            }
        }
    }

    Some(ParsedGrenade {
        id: id.to_string(),
        kind,
        owner: grenade.get("owner").and_then(|v| v.as_str()).map(|s| s.to_string()),
        velocity,
        position,
        lifetime,
        effecttime: None,
        flames: None,
        rest,
    })
}

pub fn parse_grenades(grenades: Option<&Value>) -> Vec<ParsedGrenade> {
    let Some(obj) = grenades.and_then(|v| v.as_object()) else {
        return vec![];
    };

    obj.iter()
        .filter_map(|(id, grenade)| parse_grenade(grenade, id))
        .collect()
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RoundWin {
    pub team: ParsedTeam,
    pub round: i32,
    pub side: String,
    pub outcome: String,
}

pub fn get_round_win(
    map_round: i32,
    teams_ct: &ParsedTeam,
    teams_t: &ParsedTeam,
    round_wins: &HashMap<String, String>,
    round: i32,
    regulation_mr: i32,
    overtime_mr: i32,
) -> Option<RoundWin> {
    let mut index_round = round;

    if map_round > 2 * regulation_mr {
        let max_overtime_rounds = 2 * overtime_mr
            * ((map_round - (2 * regulation_mr + 1)) / (2 * overtime_mr))
            + 2 * regulation_mr;

        if round <= max_overtime_rounds {
            return None;
        }

        let round_in_ot = (round - (2 * regulation_mr + 1)).rem_euclid(overtime_mr * 2) + 1;
        index_round = round_in_ot;
    }

    let key = index_round.to_string();
    let round_outcome = round_wins.get(&key)?.clone();

    let win_side = round_outcome
        .split('_')
        .next()
        .unwrap_or("")
        .to_uppercase();

    let mut result = RoundWin {
        team: teams_ct.clone(),
        round,
        side: win_side.clone(),
        outcome: round_outcome,
    };

    if did_team_win_that_round(teams_ct, round, &win_side, map_round, regulation_mr, overtime_mr) {
        return Some(result);
    }

    result.team = teams_t.clone();
    Some(result)
}

pub fn parse_vec3_from_opt_str(v: Option<&Value>) -> Option<Vec<f64>> {
    let s = v?.as_str()?;
    Some(
        s.split(", ")
            .map(|p| p.parse::<f64>().unwrap_or(0.0))
            .collect(),
    )
}

pub fn parse_f64_from_str(v: Option<&Value>) -> Option<f64> {
    v.and_then(|x| x.as_str())
        .and_then(|s| s.parse::<f64>().ok())
}
