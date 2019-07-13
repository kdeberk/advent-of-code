const SIZE:usize = 300;
const SERIAL:usize = 8772;

fn part_1(sums: &[[i32; SIZE]; SIZE]) -> (usize, usize) {
    let mut max = std::i32::MIN;
    let mut max_coordinate = (0, 0);

    for x in 0..SIZE - 3 {
        for y in 0..SIZE - 3 {
            // See: https://en.wikipedia.org/wiki/Summed-area_table
            // the sum of all values within the rectangle bounded by the corners
            //    A------B
            //    |      |
            //    |      |
            //    C------D
            // will be A+D-B-C

            let sum = sums[y][x] + sums[y + 3][x + 3] - sums[y + 3][x] - sums[y][x + 3];

            if sum > max {
                max = sum;
                max_coordinate = (x + 1, y + 1);
            }
        }
    }

    max_coordinate
}


fn part_2(sums: &[[i32; SIZE]; SIZE]) -> (usize, usize, usize) {
    let mut max = std::i32::MIN;
    let mut max_coordinate = (0, 0, 0);

    for size in 0..SIZE {
        for x in 0..SIZE - size {
            for y in 0..SIZE - size {
                let sum = sums[y][x] + sums[y + size][x + size] - sums[y + size][x] - sums[y][x + size];

                if sum > max {
                    max = sum;
                    max_coordinate = (x + 1, y + 1, size);
                }
            }
        }
    }

    max_coordinate
}


pub fn solve(_lines: Vec<String>) {
    let mut sums:[[i32; SIZE]; SIZE] = [[0; SIZE]; SIZE];

    // each cell in the sums contains the sums of all the cells that have a smaller x and/or smaller y value.

    for x in 0..SIZE {
        let rack_id = 10 + x;

        for y in 0..SIZE {
            let power_level = rack_id * (rack_id * y + SERIAL);
            let fuel_cell = ((power_level % 1_000) / 100) as i32 - 5;

            if x > 0 && y > 0 {
                // subtract sums[y - 1][x - 1] otherwise sums[y][x] will store twice that value through sums[y - 1][x] + sums[y][x - 1]
                sums[y][x] = fuel_cell + sums[y - 1][x] + sums[y][x - 1] - sums[y - 1][x - 1]
            } else if x > 0 {
                sums[y][x] = fuel_cell + sums[y][x - 1]
            } else if y > 0 {
                sums[y][x] = fuel_cell + sums[y - 1][x]
            }
        }
    }

    println!("Day 11");
    println!("Part 1: {:?}", part_1(&sums));
    println!("Part 2: {:?}", part_2(&sums));
}
