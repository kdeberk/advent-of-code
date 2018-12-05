
use std::cmp::Ordering;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

#[derive(Eq, PartialEq, PartialOrd)]
struct Date {
    minute: u32,
    hour: u32,
    day: u32,
    month: u32,
}

impl Ord for Date {
    fn cmp(&self, other: &Date) -> Ordering {
        for (a, b) in [self.month, self.day, self.hour, self.minute].iter().zip(&[other.month, other.day, other.hour, other.minute]) {
            if a > b {
                return Ordering::Greater
            } else if a < b {
                return Ordering::Less
            }
        }
        Ordering::Equal
    }
}

#[derive(Eq, PartialEq)]
enum Observation {
    Start { date: Date, guard_id: u32 },
    Sleep { date: Date },
    Awake { date: Date },
}

impl Observation {
    fn date(&self) -> &Date {
        match self {
            Observation::Start { date, guard_id: _guard_id } => date,
            Observation::Sleep { date } => date,
            Observation::Awake { date } => date,
        }
    }
}

impl Ord for Observation {
    fn cmp(&self, other: &Observation) -> Ordering {
        self.date().cmp(other.date())
    }
}

impl PartialOrd for Observation {
    fn partial_cmp(&self, other: &Observation) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Eq, PartialEq)]
struct Sleep {
    start: u32,
    end: u32,
}

#[derive(Clone, Eq, PartialEq)]
struct Guard {
    id: u32,
    sleeps: Vec<Sleep>,
}

impl Guard {
    fn new(guard_id: u32) -> Self {
        Self { id: guard_id, sleeps: vec![] }
    }

    fn minutes_asleep(&self) -> u32 {
        let mut count = 0;

        for Sleep { start, end } in self.sleeps.iter() {
            count += end - start
        }

        count
    }

    fn minute_most_often_asleep(&self) -> u32 {
        let mut minutes = [0u32; 60];

        for Sleep { start, end } in self.sleeps.iter() {
            for i in *start..*end {
                minutes[i as usize] += 1;
            }
        }

        let mut minute = 0;
        for i in 1..60 {
            if minutes[i] > minutes[minute] {
                minute = i;
            }
        }

        minute as u32
    }

    fn n_times_asleep_at_minute(&self, minute: u32) -> u32 {
        let mut count = 0;
        for Sleep { start, end } in self.sleeps.iter() {
            if *start <= minute && minute < *end {
                count += 1;
            }
        }
        count
    }
}

fn parse(string: &str) -> u32 {
    string.parse().unwrap()
}

fn read_entries(filename: &str) -> Vec<Observation> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);

    let mut observations:Vec<Observation> = vec![];

    for line in reader.lines() {
        let line = line.unwrap();
        let parts:Vec<&str> = line.split([' ', '[', ']', '#', ':', '-'].as_ref()).filter(|part| 0 < part.trim().len()).collect();

        let date = Date { month: parse(parts[1]), day: parse(parts[2]), hour: parse(parts[3]), minute: parse(parts[4]), };

        observations.push(
            if line.contains("begins shift") {
                Observation::Start { date: date, guard_id: parts[6].parse().unwrap() }
            } else if line.contains("falls asleep") {
                Observation::Sleep { date: date }
            } else if line.contains("wakes up") {
                Observation::Awake { date: date }
            } else {
                panic!("Action not known")
            })
    }

    observations.sort();
    observations
}

fn read_guard_shifts(observations: Vec<Observation>) -> Vec<Guard> {
    let mut guards:HashMap<u32, Guard> = HashMap::new();
    let mut current_guard_id:Option<u32> = None;
    let mut current_sleep_start:Option<u32> = None;

    for observation in observations.iter() {
        match observation {
            Observation::Start { date: _date, guard_id } => {
                if let Some(sleep_start) = current_sleep_start {
                    if let Some(mut guard) = guards.get_mut(&current_guard_id.unwrap()) {
                        guard.sleeps.push(Sleep { start: sleep_start, end: 60 });
                    } else {
                        panic!("Who was sleeping?");
                    }
                }

                guards.entry(*guard_id).or_insert(Guard::new(*guard_id));
                current_guard_id = Some(*guard_id);
                current_sleep_start = None;
            },
            Observation::Sleep { date } => {
                current_sleep_start = Some(date.minute);
            },
            Observation::Awake { date } => {
                if let Some(sleep_start) = current_sleep_start {
                    if let Some(mut guard) = guards.get_mut(&current_guard_id.unwrap()) {
                        guard.sleeps.push(Sleep { start: sleep_start, end: date.minute });
                        current_sleep_start = None;
                    } else {
                        panic!("Who was sleeping?")
                    }
                } else {
                    panic!("Not sleeping?")
                }
            }
        }
    }

    guards.into_iter().map (|(_k, v)| v).collect::<Vec<Guard>>()
}

fn part_1(guards: &Vec<Guard>) {
    let mut most_asleep_guard = &guards[0];

    for guard in guards[1..].iter() {
        if guard.minutes_asleep() > most_asleep_guard.minutes_asleep() {
            most_asleep_guard = guard;
        }
    }

    println!("{}", most_asleep_guard.id * most_asleep_guard.minute_most_often_asleep());
}

fn part_2(guards: &Vec<Guard>) {
    let mut most_asleep_guard = &guards[0];

    for guard in guards[1..].iter() {
        if guard.n_times_asleep_at_minute(guard.minute_most_often_asleep()) >
            most_asleep_guard.n_times_asleep_at_minute(most_asleep_guard.minute_most_often_asleep())
        {
            most_asleep_guard = guard;
        }
    }

    println!("{}", most_asleep_guard.id * most_asleep_guard.minute_most_often_asleep());
}

fn main() {
    let guards = read_guard_shifts(read_entries("input"));

    part_1(&guards);
    part_2(&guards);
}
