use std::collections::VecDeque;

#[derive(Debug)]
struct Node {
    children: Vec<Node>,
    metadata: Vec<u32>
}

impl Node {
    fn sum_metadata(&self) -> u32 {
        let mut sum = 0;

        sum += self.children.iter().fold(0, |acc, cur| acc + cur.sum_metadata());
        sum += self.metadata.iter().fold(0, |acc, cur| acc + cur);
        sum
    }

    fn special_sum(&self) -> u32 {
        if 0 == self.children.len() {
            self.metadata.iter().fold(0, |acc, cur| acc + cur)            
        } else {
            let mut sum = 0;

            for &metadatum in self.metadata.iter() {
                if 0 < metadatum && metadatum <= self.children.len() as u32 {
                    sum += self.children[(metadatum - 1) as usize].special_sum();
                }
            }
            sum
        }
    }
}


fn read_node(stream: &mut VecDeque<u32>) -> Node {
    let n_children:u32 = stream.pop_front().unwrap();
    let n_metadata:u32 = stream.pop_front().unwrap();

    let mut node = Node { children: vec![], metadata: vec![] };
    for _ in 0..n_children {
        node.children.push(read_node(stream));
    }
    for _ in 0..n_metadata {
        node.metadata.push(stream.pop_front().unwrap());
    }

    node
}


fn construct_tree(lines: Vec<String>) -> Node {
    let line = lines.get(0).unwrap();
    let mut parts:VecDeque<u32> = line.split(" ").map(|part| part.trim().parse().unwrap()).collect();

    read_node(&mut parts)
}

fn part_1(tree: &Node) -> u32 {
    tree.sum_metadata()
}

fn part_2(tree: &Node) -> u32 {
    tree.special_sum()
}

pub fn solve(lines: Vec<String>) {
    let tree = construct_tree(lines);

    println!("Day 8");
    println!("Part 1: {}", part_1(&tree));
    println!("Part 2: {}", part_2(&tree));
}
