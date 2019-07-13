fn part_1(input: usize) -> String {
    let mut recipes:Vec<u8> = vec![3, 7];
    let mut elf0:usize = 0;
    let mut elf1:usize = 1;

    for _ in 0..(input + 10) {
        let sum = recipes[elf0] + recipes[elf1];
        if sum >= 10 {
            recipes.push(sum / 10);
        }
        recipes.push(sum % 10);

        elf0 = (1 + elf0 + recipes[elf0] as usize) % recipes.len();
        elf1 = (1 + elf1 + recipes[elf1] as usize) % recipes.len();
    }

    recipes[input..10 + input]
        .iter()
        .map(|i| format!("{}", i))
        .collect::<Vec<String>>()
        .join("")
}

fn digits(int: usize) -> Vec<u8> {
    let mut digits = vec![];
    let mut int = int;

    while int > 0 {
        digits.push((int % 10) as u8);
        int /= 10;
    }
    digits.reverse();
    digits
}

fn ends_with(recipes: &Vec<u8>, ends_with: &Vec<u8>) -> bool {
    if recipes.len() < ends_with.len() {
        return false
    }

    &recipes[recipes.len() - ends_with.len()..] == ends_with.as_slice()
}

fn part_2(input: usize) -> usize {
    let mut recipes:Vec<u8> = vec![3, 7];
    let mut elf0:usize = 0;
    let mut elf1:usize = 1;
    let to_find = digits(input);

    for _ in 0.. {
        let sum = recipes[elf0] + recipes[elf1];
        if sum >= 10 {
            recipes.push(sum / 10);
            if ends_with(&recipes, &to_find) {
                break;
            }
        }

        recipes.push(sum % 10);
        if ends_with(&recipes, &to_find) {
            break
        }

        elf0 = (1 + elf0 + recipes[elf0] as usize) % recipes.len();
        elf1 = (1 + elf1 + recipes[elf1] as usize) % recipes.len();
    }

    recipes.len() - to_find.len()
}

pub fn solve(_lines: Vec<String>) {
    let input = 640441;

    println!("Day 14");
    println!("Part 1: {}", part_1(input));
    println!("Part 2: {}", part_2(input));
}
