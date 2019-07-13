use std::collections::{HashMap, HashSet, VecDeque};
use std::env;

struct Facility {
    doors: HashMap<(isize, isize), HashSet<(isize, isize)>>,
}

impl Facility {
    pub fn connect_rooms(&mut self, a: (isize, isize), b: (isize, isize)) {
        self._connect(a, b);
        self._connect(b, a);
    }

    pub fn neighboring_rooms(&self, room: &(isize, isize)) -> &HashSet<(isize, isize)> {
        self.doors.get(&room).unwrap()
    }

    fn _connect(&mut self, a: (isize, isize), b: (isize, isize)) {
        self.doors.entry(a)
            .and_modify(|hs| { hs.insert(b); })
            .or_insert({
                let mut hs = HashSet::new();
                hs.insert(b);
                hs
            });
    }
}

fn construct_facility(lines: Vec<String>) -> Facility {
    let regex = lines.iter().next().unwrap();

    let mut facility = Facility { doors: HashMap::new() };
    let mut junctions:VecDeque<(isize, isize)> = VecDeque::new();

    let mut current_room = (0, 0);
    for ch in regex.chars() {
        match ch {
            '(' => { junctions.push_back(current_room); },
            ')' => { current_room = junctions.pop_back().unwrap(); },
            '|' => { current_room = *junctions.back().unwrap(); }
            'N' | 'W' | 'E' | 'S' => {
                let next_room = match ch {
                    'N' => (current_room.0, current_room.1 + 1),
                    'W' => (current_room.0 - 1, current_room.1),
                    'E' => (current_room.0 + 1, current_room.1),
                    'S' => (current_room.0, current_room.1 - 1),
                    _ => panic!("Huh!?!?)"),
                };

                facility.connect_rooms(current_room, next_room);
                current_room = next_room;
            },
            '$' | '^' => (),
            _ => { panic!("unknown char {}", ch) },
        }
    }
    facility
}

fn draw_map(facility: &Facility) {
    let mut min_x = std::isize::MAX;
    let mut max_x = std::isize::MIN;
    let mut min_y = std::isize::MAX;
    let mut max_y = std::isize::MIN;

    for ((x, y), _) in facility.doors.iter() {
        if *x < min_x { min_x = *x }
        if max_x < *x { max_x = *x }
        if *y < min_y { min_y = *y }
        if max_y < *y { max_y = *y }
    }

    let yrange = (max_y - min_y + 1) * 2 + 1;
    let xrange = (max_x - min_x + 1) * 2 + 1;

    let mut room_y = max_y;
    for y in 0..yrange {
        if 0 == y || yrange - 1 == y {
            print!("{}", "#".repeat(xrange as usize));
        } else if 0 == y % 2 {
            let mut room_x = min_x;

            for x in 0..xrange {
                if 0 == x % 2 {
                    print!("#");
                } else {
                    let up_room = (room_x, room_y);
                    let down_room = (room_x, room_y + 1);

                    if facility.doors.contains_key(&up_room) && facility.doors.get(&up_room).unwrap().contains(&down_room) {
                        print!("-");
                    } else {
                        print!("#");
                    }
                    room_x += 1;
                }
            }
        } else {
            let mut room_x = min_x;

            for x in 0..xrange {
                if 0 == x {
                    print!("#")
                } else if 0 == x % 2 {
                    let left_room = (room_x, room_y);
                    let right_room = (room_x + 1, room_y);
                    
                    if facility.doors.contains_key(&left_room) && facility.doors.get(&left_room).unwrap().contains(&right_room) {
                        print!("|");
                    } else {
                        print!("#");
                    }
                    room_x += 1;
                } else if 0 == room_x && 0 == room_y {
                    print!("X");
                } else {
                    print!(" ");
                }
            }
            room_y -= 1;
        }
        println!("")
    }
}

fn find_most_distant_room(facility: &Facility) -> usize {
    let mut seen:HashSet<(isize, isize)> = HashSet::new();
    let mut queue:VecDeque<((isize, isize), usize)> = VecDeque::new();
    let mut max_distance = std::usize::MIN;

    queue.push_back(((0, 0), 0));

    while ! queue.is_empty() {
        let (current, distance) = queue.pop_front().unwrap();
        max_distance = usize::max(distance, max_distance);

        if seen.contains(&current) { continue }
        for other_room in facility.neighboring_rooms(&current).iter() {
            if seen.contains(other_room) { continue }
            queue.push_back((*other_room, distance + 1));
        }
        seen.insert(current);
    }
    max_distance
}

fn count_rooms_with_greater_distance_than(facility: &Facility, limit: usize) -> usize {
    let mut count = 0;
    let mut seen:HashSet<(isize, isize)> = HashSet::new();
    let mut queue:VecDeque<((isize, isize), usize)> = VecDeque::new();

    queue.push_back(((0, 0), 0));

    while ! queue.is_empty() {
        let (current, distance) = queue.pop_front().unwrap();
        if distance >= limit {
            count += 1
        }

        if seen.contains(&current) { continue }
        for other_room in facility.neighboring_rooms(&current).iter() {
            if seen.contains(other_room) { continue }
            queue.push_back((*other_room, distance + 1));
        }
        seen.insert(current);
    }
    count
}

fn part_1(facility: &Facility) -> usize {
    if env::var("DRAW").is_ok() { draw_map(&facility); }
    find_most_distant_room(facility)
}

fn part_2(facility: &Facility) -> usize {
    if env::var("DRAW").is_ok() { draw_map(&facility); }
    count_rooms_with_greater_distance_than(&facility, 1000)
}

pub fn solve(lines: Vec<String>) {
    // assert!(3 == part_1(&construct_facility("1.test")));
    // assert!(10 == part_1(&construct_facility("2.test")));
    // assert!(18 == part_1(&construct_facility("3.test")));
    // assert!(23 == part_1(&construct_facility("4.test")));
    // assert!(31 == part_1(&construct_facility("5.test")));

    let facility = construct_facility(lines);

    println!("Day 20");
    println!("Part 1: {}", part_1(&facility));
    println!("Part 2: {}", part_2(&facility));
}
