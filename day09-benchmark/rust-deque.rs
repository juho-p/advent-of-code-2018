use std::collections::VecDeque;

type IntDeque = VecDeque<u32>;

fn normal_turn(v: &mut IntDeque, next: u32) {
    let first = v.pop_front().unwrap();
    let second = v.pop_front().unwrap();
    v.push_front(next);
    v.push_back(first);
    v.push_back(second);
}

fn special_turn(v: &mut IntDeque) -> u32 {
    for _ in 0..6 {
        let back = v.pop_back().unwrap();
        v.push_front(back);
    }

    v.pop_back().unwrap()
}

fn take_turn(player: u32, next: u32, score: &mut Vec<u32>, v: &mut IntDeque) {
    if next % 23 == 0 {
        let removed = special_turn(v);
        score[player as usize] += next + removed;
    } else {
        normal_turn(v, next);
    }
}

fn high_score(player_count: usize, last_marble: u32) -> u32 {
    let mut score = vec![0; player_count as usize];

    let mut v = IntDeque::new();
    v.push_back(1);
    v.push_back(0);

    let mut marble: u32 = 2;
    loop {
        for player in 0..player_count {
            take_turn(player as u32, marble, &mut score, &mut v);

            marble += 1;

            if marble == last_marble {
                return score.into_iter().max().unwrap();
            }
        }
    }
}

fn main() {
    println!("{}", high_score(410, 72059));
    println!("{}", high_score(410, 7205900));
}
