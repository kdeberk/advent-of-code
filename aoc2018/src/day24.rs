use std::cmp::Ordering;
use std::collections::HashSet;
use std::cell::{Ref, RefCell, RefMut};
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Faction {
    ImmuneSystem,
    Infection,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum AttackType {
    Bludgeoning,
    Cold,
    Fire,
    Radiation,
    Slashing,
}

impl AttackType {
    fn from_string(string: &str) -> Option<Self> {
        match string {
            "bludgeoning" => Some(AttackType::Bludgeoning),
            "cold" => Some(AttackType::Cold),
            "fire" => Some(AttackType::Fire),
            "radiation" => Some(AttackType::Radiation),
            "slashing" => Some(AttackType::Slashing),
            _ => panic!("Unknown attack type {}", string),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Group {
    n_units: usize,
    n_hitpoints: usize,
    initiative: usize,
    immune: Vec<AttackType>,
    weakness: Vec<AttackType>,
    attack_type: AttackType,
    attack_damage: usize,
    faction: Faction,
    name: String,
}

impl Group {
    fn from_line(i: usize, faction: Faction, line: &str) -> Self {
        let substrings = line.split(['(', ')'].as_ref()).map(|part| part.trim()).collect::<Vec<&str>>();
        let (immunes, weakness) = if 3 == substrings.len() { Self::immunes_weakness_from_line(substrings[1]) } else { (vec![], vec![]) };

        let mut line:String = substrings[0].to_owned();
        if 3 == substrings.len() {
            line.push_str(" ");
            line.push_str(substrings[2]);
        }
        let words = line.split(' ').collect::<Vec<&str>>();

        Group {
            n_units: words[0].parse().unwrap(),
            n_hitpoints: words[4].parse().unwrap(),
            initiative: words[17].parse().unwrap(),
            immune: immunes,
            weakness: weakness,
            attack_type: AttackType::from_string(words[13]).unwrap(),
            attack_damage: words[12].parse().unwrap(),
            faction: faction.clone(),
            name: format!("{} group {}", if Faction::ImmuneSystem == faction { "Immune System" } else { "Infection" }, i),
        }
    }

    fn immunes_weakness_from_line(line: &str) -> (Vec<AttackType>, Vec<AttackType>) {
        let substrings = line.split(';').map(|substring| substring.trim()).collect::<Vec<&str>>();
        let mut immune = vec![];
        let mut weakness = vec![];

        for substring in substrings {
            let vec = if substring.starts_with("immune to") { &mut immune } else { &mut weakness };
            let words = substring.split([' ', ','].as_ref())
                .map(|word| word.trim())
                .filter(|word| 0 < word.len())
                .collect::<Vec<&str>>();

            for word in words[2..].iter() {
                if let Some(attack_type) = AttackType::from_string(word) {
                    vec.push(attack_type)
                }
            }
        }
        (immune, weakness)
    }

    fn effective_power(&self) -> usize {
        self.n_units * self.attack_damage
    }

    fn choose<'a>(&self, all_groups: &mut HashSet<&'a GroupRef>) -> Option<&'a GroupRef> {
        let mut result = None;

        for candidate in all_groups.iter() {
            let candidate_damage = self.damage_inflicted_on(&candidate);
            let candidate_power = candidate.borrow().effective_power();
            let candidate_initiative = candidate.borrow().initiative;

            if 0 == candidate_damage { continue }
            if self.faction == candidate.borrow().faction { continue }

            match result {
                None => { result = Some(candidate.clone()) },
                Some(current) => {
                    let current_damage = self.damage_inflicted_on(&current);
                    let current_power = current.borrow().effective_power();
                    let current_initiative = current.borrow().initiative;

                    if current_damage < candidate_damage {
                        result = Some(candidate.clone())
                    } else if current_damage == candidate_damage && current_power < candidate_power {
                        result = Some(candidate.clone())
                    } else if current_damage == candidate_damage && current_power == candidate_power && current_initiative < candidate_initiative {
                        result = Some(candidate.clone())
                    }
                }
            }
        }

        if let Some(group) = result {
            all_groups.take(group)
        } else {
            None
        }
    }

    fn damage_inflicted_on(&self, enemy_group: &GroupRef) -> usize {
        let enemy_group = enemy_group.borrow();

        if enemy_group.immune.contains(&self.attack_type) {
            0
        } else if enemy_group.weakness.contains(&self.attack_type) {
            2 * self.effective_power()
        } else  {
            self.effective_power()
        }
    }

    fn receive_damage(&mut self, damage: usize) {
        if damage > self.n_units * self.n_hitpoints {
            self.n_units = 0
        } else {
            self.n_units -= damage / self.n_hitpoints
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct GroupRef {
    reference: RefCell<Group>
}

impl Hash for GroupRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.reference.borrow().hash(state)
    }
}

impl GroupRef {
    fn new(group: Group) -> Self {
        Self { reference: RefCell::new(group) }
    }

    fn borrow(&self) -> Ref<Group> {
        self.reference.borrow()
    }

    fn borrow_mut(&self) -> RefMut<Group> {
        self.reference.borrow_mut()
    }
}

fn sort_groups_by_power_and_initiative(groups: &mut Vec<&GroupRef>) {
    groups.sort_by(|a, b| {
        let a = a.borrow();
        let b = b.borrow();

        if a.effective_power() < b.effective_power() {
            Ordering::Greater
        } else if a.effective_power() == b.effective_power() && a.initiative < b.initiative {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    });
}

fn sort_attack_plan_by_attacker_initiative(attack_plan: &mut Vec<(&GroupRef, &GroupRef)>) {
    attack_plan.sort_by(|(a, _), (b, _)| {
        let a = a.borrow();
        let b = b.borrow();

        if a.initiative < b.initiative {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    });
}

fn target_selection<'a>(armies: &'a Vec<GroupRef>) -> Vec<(&'a GroupRef, &'a GroupRef)> {
    let mut available_groups:HashSet<&GroupRef> = HashSet::new();
    for group in armies.iter() { available_groups.insert(group); }

    let mut groups_by_effective_power = armies.iter().collect::<Vec<&GroupRef>>();
    sort_groups_by_power_and_initiative(&mut groups_by_effective_power);

    let mut attack_plan = vec![];
    for group in groups_by_effective_power {
        if let Some(chosen) = group.borrow().choose(&mut available_groups) {
            attack_plan.push((group, chosen))
        }
    }
    sort_attack_plan_by_attacker_initiative(&mut attack_plan);
    attack_plan
}

fn attack(attack_plan: &Vec<(&GroupRef, &GroupRef)>) {
    for (attacker, defender) in attack_plan {
        let damage = attacker.borrow().damage_inflicted_on(&defender);

        defender.borrow_mut().receive_damage(damage);
    }
}

fn unit_count(groups: &Vec<GroupRef>) -> usize {
    groups.iter().map(|group| group.borrow().n_units).fold(0, |cur, acc| cur + acc)
}

fn battle_is_over(groups: &Vec<GroupRef>) -> bool {
    ! groups.iter().any(|group| group.borrow().faction == Faction::ImmuneSystem) ||
        ! groups.iter().any(|group| group.borrow().faction == Faction::Infection)
}

fn fight(armies: &mut Vec<GroupRef>) {
    loop {
        {
            let attack_plan = target_selection(&armies);
            if 0 == attack_plan.len() { break } // Everyone is immune to everyone else

            let n_units = unit_count(&armies);
            attack(&attack_plan);
            if n_units == unit_count(&armies) { break } // Not enough damage was incurred by any side to make a difference
        }

        armies.retain(|group| 0 < group.borrow().n_units);
        if battle_is_over(&armies) { break }
    }
}

fn part_1(armies: &Vec<GroupRef>) -> usize {
    let mut armies:Vec<GroupRef> = armies.clone();

    fight(&mut armies);

    armies.into_iter().map(|group| group.borrow().n_units).fold(0, |cur, acc| cur + acc)
}

fn immune_system_won(groups: &Vec<GroupRef>) -> bool {
    0 < groups.len() && ! groups.iter().any(|group| group.borrow().faction == Faction::Infection)
}

fn part_2(armies: &Vec<GroupRef>) -> usize {
    let mut boost = 1;

    loop {
        let mut armies:Vec<GroupRef> = armies.clone();

        for group in armies.iter() {
            if group.borrow().faction == Faction::ImmuneSystem {
                group.borrow_mut().attack_damage += boost
            }
        }

        fight(&mut armies);

        if immune_system_won(&armies) {
            return unit_count(&armies);
        }
        boost += 1
    }
}

fn test_armies() -> Vec<GroupRef> {
    vec![
        GroupRef::new(Group::from_line(1, Faction::ImmuneSystem, "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2")),
        GroupRef::new(Group::from_line(2, Faction::ImmuneSystem, "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3")),
        GroupRef::new(Group::from_line(1, Faction::Infection, "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1")),
        GroupRef::new(Group::from_line(2, Faction::Infection, "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")),
    ]
}

fn input_armies() -> Vec<GroupRef> {
    vec![
        GroupRef::new(Group::from_line(1, Faction::ImmuneSystem, "7056 units each with 8028 hit points (weak to radiation) with an attack that does 10 slashing damage at initiative 13")),
        GroupRef::new(Group::from_line(2, Faction::ImmuneSystem, "4459 units each with 10339 hit points (immune to fire, radiation, slashing) with an attack that does 22 cold damage at initiative 4")),
        GroupRef::new(Group::from_line(3, Faction::ImmuneSystem, "724 units each with 10689 hit points (immune to bludgeoning, cold, fire) with an attack that does 124 radiation damage at initiative 17")),
        GroupRef::new(Group::from_line(4, Faction::ImmuneSystem, "1889 units each with 3361 hit points (weak to cold) with an attack that does 17 fire damage at initiative 2")),
        GroupRef::new(Group::from_line(5, Faction::ImmuneSystem, "4655 units each with 1499 hit points (weak to fire) with an attack that does 2 fire damage at initiative 5")),
        GroupRef::new(Group::from_line(6, Faction::ImmuneSystem, "6799 units each with 3314 hit points with an attack that does 4 radiation damage at initiative 16")),
        GroupRef::new(Group::from_line(7, Faction::ImmuneSystem, "2407 units each with 4016 hit points (weak to slashing; immune to bludgeoning) with an attack that does 13 fire damage at initiative 20")),
        GroupRef::new(Group::from_line(8, Faction::ImmuneSystem, "5372 units each with 5729 hit points with an attack that does 9 fire damage at initiative 14")),
        GroupRef::new(Group::from_line(9, Faction::ImmuneSystem, "432 units each with 11056 hit points with an attack that does 220 cold damage at initiative 10")),
        GroupRef::new(Group::from_line(10, Faction::ImmuneSystem, "3192 units each with 8960 hit points (weak to slashing, radiation) with an attack that does 24 cold damage at initiative 15")),

        GroupRef::new(Group::from_line(1, Faction::Infection, "4052 units each with 25687 hit points (weak to fire, radiation) with an attack that does 11 slashing damage at initiative 18")),
        GroupRef::new(Group::from_line(2, Faction::Infection, "1038 units each with 13648 hit points (weak to slashing) with an attack that does 24 bludgeoning damage at initiative 9")),
        GroupRef::new(Group::from_line(3, Faction::Infection, "6627 units each with 34156 hit points (weak to radiation) with an attack that does 10 slashing damage at initiative 6")),
        GroupRef::new(Group::from_line(4, Faction::Infection, "2299 units each with 45224 hit points (weak to fire) with an attack that does 38 cold damage at initiative 19")),
        GroupRef::new(Group::from_line(5, Faction::Infection, "2913 units each with 30594 hit points (weak to radiation; immune to cold) with an attack that does 20 fire damage at initiative 1")),
        GroupRef::new(Group::from_line(6, Faction::Infection, "2153 units each with 14838 hit points (immune to fire, bludgeoning, radiation; weak to slashing) with an attack that does 11 radiation damage at initiative 3")),
        GroupRef::new(Group::from_line(7, Faction::Infection, "2381 units each with 61130 hit points (weak to cold) with an attack that does 39 slashing damage at initiative 8")),
        GroupRef::new(Group::from_line(8, Faction::Infection, "2729 units each with 33834 hit points (immune to slashing, cold) with an attack that does 23 fire damage at initiative 7")),
        GroupRef::new(Group::from_line(9, Faction::Infection, "344 units each with 20830 hit points (immune to fire) with an attack that does 116 bludgeoning damage at initiative 12")),
        GroupRef::new(Group::from_line(10, Faction::Infection, "6848 units each with 50757 hit points with an attack that does 12 slashing damage at initiative 11")),
    ]
}

pub fn solve(_lines: Vec<String>) {
    println!("Day 24");
    assert!(5216 == part_1(&test_armies()));
    println!("Part 1: {}", part_1(&input_armies()));

    assert!(51 == part_2(&test_armies()));
    println!("Part 2: {}", part_2(&input_armies()));
}
