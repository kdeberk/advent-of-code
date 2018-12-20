use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn construct_doors(regex: &str) -> HashMap<(isize, isize), HashSet<(isize, isize)>> {
    let mut current_room = (0, 0);
    let mut doors:HashMap<(isize, isize), HashSet<(isize, isize)>> = HashMap::new();
    let mut junctions:VecDeque<(isize, isize)> = VecDeque::new();

    for ch in regex.chars() {
        match ch {
            '(' => {
                junctions.push_back(current_room);
            },
            ')' => {
                current_room = junctions.pop_back().unwrap();
            },
            '|' => {
                current_room = *junctions.back().unwrap();
            }
            '$' | '^' => (),
            'N' | 'W' | 'E' | 'S' => {
                let next_room = match ch {
                    'N' => (current_room.0, current_room.1 + 1),
                    'W' => (current_room.0 - 1, current_room.1),
                    'E' => (current_room.0 + 1, current_room.1),
                    'S' => (current_room.0, current_room.1 - 1),
                    _ => panic!("Huh!?!?)"),
                };

                doors.entry(current_room)
                    .and_modify(|hs| { hs.insert(next_room); })
                    .or_insert({
                        let mut hs = HashSet::new();
                        hs.insert(next_room);
                        hs
                    });
                doors.entry(next_room)
                    .and_modify(|hs| { hs.insert(current_room); })
                    .or_insert({
                        let mut hs = HashSet::new();
                        hs.insert(current_room);
                        hs
                    });
                current_room = next_room;
            },
            _ => { panic!("unknown char {}", ch) },
        }
    }
    doors
}

fn draw_map(doors: &HashMap<(isize, isize), HashSet<(isize, isize)>>) {
    let mut min_x = std::isize::MAX;
    let mut max_x = std::isize::MIN;
    let mut min_y = std::isize::MAX;
    let mut max_y = std::isize::MIN;

    for ((x, y), _) in doors.iter() {
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

                    if doors.contains_key(&up_room) && doors.get(&up_room).unwrap().contains(&down_room) {
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
                    
                    if doors.contains_key(&left_room) && doors.get(&left_room).unwrap().contains(&right_room) {
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

fn find_max_distant_room(doors:&HashMap<(isize, isize), HashSet<(isize, isize)>>) -> usize {
    let mut seen:HashSet<(isize, isize)> = HashSet::new();
    let mut queue:VecDeque<((isize, isize), usize)> = VecDeque::new();
    let mut max_distance = std::usize::MIN;

    queue.push_back(((0, 0), 0));

    while ! queue.is_empty() {
        let (current, distance) = queue.pop_front().unwrap();
        max_distance = usize::max(distance, max_distance);

        if seen.contains(&current) {
            continue;
        } else {
            let other_rooms = doors.get(&current).unwrap();
            for other_room in other_rooms.iter() {
                if seen.contains(other_room) {
                    continue
                } else {
                    queue.push_back((*other_room, distance + 1));
                }
            }

            seen.insert(current);
        }
    }
    max_distance
}

fn count_rooms_with_greater_distance_than(doors: &HashMap<(isize, isize), HashSet<(isize, isize)>>, limit: usize) -> usize {
    let mut count = 0;
    let mut seen:HashSet<(isize, isize)> = HashSet::new();
    let mut queue:VecDeque<((isize, isize), usize)> = VecDeque::new();

    queue.push_back(((0, 0), 0));

    while ! queue.is_empty() {
        let (current, distance) = queue.pop_front().unwrap();
        if distance >= limit {
            count += 1
        }

        if seen.contains(&current) {
            continue;
        } else {
            let other_rooms = doors.get(&current).unwrap();
            for other_room in other_rooms.iter() {
                if seen.contains(other_room) {
                    continue
                } else {
                    queue.push_back((*other_room, distance + 1));
                }
            }

            seen.insert(current);
        }
    }
    count
}

fn part_1(regex: &str) -> usize {
    let doors = construct_doors(regex);
    if env::var("DRAW").is_ok() { draw_map(&doors); }
    find_max_distant_room(&doors)
}

fn part_2(regex: &str) -> usize {
    let doors = construct_doors(regex);
    if env::var("DRAW").is_ok() { draw_map(&doors); }
    count_rooms_with_greater_distance_than(&doors, 1000)
}

fn main() {
    assert!(3 == part_1("^WNE$"));
    assert!(10 == part_1("^ENWWW(NEEE|SSE(EE|N))$"));
    assert!(18 == part_1("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"));
    assert!(23 == part_1("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"));
    assert!(31 == part_1("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"));

    let file = File::open("input").unwrap();
    let reader = BufReader::new(&file);
    let regex = reader.lines().next().unwrap().unwrap();
    println!("{}", part_1(&regex));
    println!("{}", part_2(&regex));
}
