#![feature(coroutines, coroutine_trait)]

use criterion::{
    async_executor::FuturesExecutor, black_box, criterion_group, criterion_main, Criterion,
};
use std::{
    error::Error,
    iter,
    ops::{Index, IndexMut},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Player {
    Min,
    Max,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Tile {
    NoTile, /* outside of the board */
    Empty,
    Stack(Player, u8),
}

/* Coordinate offsets for each neighbor in a hex grid. Neighbors can be found by adding these to our
 * current coordinates. These also represent straight line directions. */
const NEIGHBOR_OFFSETS: [(isize, isize); 6] = [(0, 1), (1, 1), (1, 0), (0, -1), (-1, -1), (-1, 0)];

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct Board {
    /* Tiles stored in row-major order. */
    tiles: Vec<Tile>,
    row_length: usize,
}

impl Index<(usize, usize)> for Board {
    type Output = Tile;

    fn index(&self, (r, q): (usize, usize)) -> &Self::Output {
        /* r = row coordinate, q = column coordinate
         * Return the tile for all valid coords in the board, but also return NoTile for all coords
         * outside the board. This way the indexing operation never panics. */
        if r < self.num_rows() && q < self.row_length {
            return &self.tiles[self.row_length * r + q];
        } else {
            return &Tile::NoTile;
        }
    }
}

impl IndexMut<(usize, usize)> for Board {
    fn index_mut(&mut self, (r, q): (usize, usize)) -> &mut Self::Output {
        return &mut self.tiles[self.row_length * r + q];
    }
}

impl Board {
    fn num_rows(&self) -> usize {
        return self.tiles.len() / self.row_length;
    }

    /* Iterates through all tiles in row-major order. */
    fn iter_row_major(&self) -> impl Iterator<Item = ((usize, usize), Tile)> + '_ {
        return self
            .tiles
            .iter()
            .enumerate()
            .map(|(index, &tile)| ((index / self.row_length, index % self.row_length), tile));
    }

    /* Parses a hexagonal grid string into a board. */
    fn parse(input: &str) -> Result<Board, Box<dyn Error>> {
        let row_strings = input
            .split("\n")
            /* Filter out whitespace-only rows. */
            .filter(|&row_string| !row_string.trim().is_empty())
            .enumerate()
            /* Indent each row so that the hexagonal grid becomes a square grid. The first row needs
             * to be indented by 0 spaces, the second by 2 spaces and so on. */
            .map(|(i, row_string)| {
                let indentation = i * 2;
                let row_indent = iter::repeat(' ').take(indentation).collect::<String>();
                return row_indent + row_string.trim_end();
            })
            .collect::<Vec<String>>();

        if row_strings.is_empty() {
            return Err("Empty board")?;
        }

        /* Column index of first board character in any row. */
        let string_begin_index = row_strings
            .iter()
            .map(|row_string| row_string.chars().take_while(|&char| char == ' ').count())
            .min()
            .unwrap_or(0)
            / 2
            * 2;
        /* Max number of tiles in any row. */
        let row_length = (row_strings
            .iter()
            .map(|row_string| row_string.len())
            .max()
            .unwrap_or(0)
            - string_begin_index
            + 3)
            / 4;
        /* Column index of last board character in any row. */
        let string_end_index = row_length * 4 + string_begin_index;

        let mut tiles = Vec::<Tile>::with_capacity(row_length * row_strings.len());

        for row_string in row_strings.iter() {
            /* The part of the row from begin index to end index, padded with spaces if needed. */
            let row_content = row_string
                .chars()
                .chain(iter::repeat(' '))
                .take(string_end_index)
                .skip(string_begin_index)
                .collect::<String>();

            /* Splitting row into 4 character pieces. */
            for tile_string in row_content
                .as_bytes()
                .chunks(4)
                .map(String::from_utf8_lossy)
            {
                let tile_content = tile_string.trim_end();

                if tile_content == "" {
                    tiles.push(Tile::NoTile);
                } else if tile_content == " 0" {
                    tiles.push(Tile::Empty);
                } else if tile_content.starts_with("+") {
                    let stack_size = tile_content[1..].parse::<u8>()?;
                    tiles.push(Tile::Stack(Player::Max, stack_size));
                } else if tile_content.starts_with("-") {
                    let stack_size = tile_content[1..].parse::<u8>()?;
                    tiles.push(Tile::Stack(Player::Min, stack_size));
                } else {
                    return Err("Invalid tile")?;
                }
            }
        }

        return Ok(Board { tiles, row_length });
    }

    fn possible_regular_moves_loops(&self, player: Player) {
        /* Iterate through all tiles. */
        for (orig_coords, tile) in self.iter_row_major() {
            /* Check if the tile is a splittable stack of this player. */
            if let Tile::Stack(p, stack_size) = tile {
                if p == player && stack_size > 1 {
                    /* Iterate through all straight line directions. */
                    for dir_offset in NEIGHBOR_OFFSETS {
                        /* Move to a direction as far as there are empty tiles. */
                        let mut coords = orig_coords;
                        loop {
                            /* Coordinates for the next tile in the direction.
                             * Hack: negative numbers cannot be added to a usize, so they are
                             * converted into usize with underflow and then added with overflow.
                             * Same as: let next_coords = coords + dir_offset */
                            let next_coords = (
                                coords.0.wrapping_add(dir_offset.0 as usize),
                                coords.1.wrapping_add(dir_offset.1 as usize),
                            );

                            /* If next tile is empty, move to that tile. */
                            if self[next_coords] == Tile::Empty {
                                coords = next_coords;
                            } else {
                                break;
                            }
                        }
                        /* Check if we actually found any empty tiles in the direction. */
                        if coords != orig_coords {
                            /* Iterate through all the ways to split the stack. */
                            for split in 1..stack_size {
                                /* Create the next board. */
                                let mut next_board = self.clone();
                                next_board[coords] = Tile::Stack(player, split);
                                next_board[orig_coords] = Tile::Stack(player, stack_size - split);

                                black_box(next_board);
                            }
                        }
                    }
                }
            }
        }
    }

    fn possible_regular_moves_iterator(&self, player: Player) {
        /* Iterate through all tiles. */
        let iterator = self
            .iter_row_major()
            /* Check if the tile is a splittable stack of this player. */
            .filter_map(move |(orig_coords, tile)| match tile {
                Tile::Stack(p, size) if p == player && size > 1 => Some((orig_coords, size)),
                _ => None,
            })
            .flat_map(move |(orig_coords, stack_size)| {
                /* Iterate through all straight line directions. */
                return NEIGHBOR_OFFSETS
                    .iter()
                    /* Move to a direction as far as there are empty tiles. */
                    .map(move |&dir_offset| {
                        let mut coords = orig_coords;
                        loop {
                            /* Coordinates for the next tile in the direction.
                             * Hack: negative numbers cannot be added to a usize, so they are
                             * converted into usize with underflow and then added with overflow.
                             * Same as: let next_coords = coords + dir_offset */
                            let next_coords = (
                                coords.0.wrapping_add(dir_offset.0 as usize),
                                coords.1.wrapping_add(dir_offset.1 as usize),
                            );

                            /* If next tile is empty, move to that tile. */
                            if self[next_coords] == Tile::Empty {
                                coords = next_coords;
                            } else {
                                break;
                            }
                        }
                        return coords;
                    })
                    /* Check if we actually found any empty tiles in the direction. */
                    .filter(move |&coords| coords != orig_coords)
                    .flat_map(move |coords| {
                        /* Iterate through all the ways to split the stack. */
                        return (1..stack_size).map(move |split| {
                            /* Create the next board. */
                            let mut next_board = self.clone();
                            next_board[coords] = Tile::Stack(player, split);
                            next_board[orig_coords] = Tile::Stack(player, stack_size - split);
                            return next_board;
                        });
                    });
            });

        for next_board in iterator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_native(&self, player: Player) {
        use std::ops::{Coroutine, CoroutineState};
        use std::pin::Pin;

        let mut coroutine = || {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield next_board;
                                }
                            }
                        }
                    }
                }
            }
        };

        while let CoroutineState::Yielded(next_board) = Pin::new(&mut coroutine).resume(()) {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_next_gen(&self, player: Player) {
        use next_gen::prelude::*;

        #[generator(yield(Board))]
        fn generate_moves(board: &Board, player: Player) {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in board.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if board[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    let mut next_board = board.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        }

        mk_gen!(let generator = generate_moves(self, player));

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_next_gen_boxed(&self, player: Player) {
        use next_gen::prelude::*;

        #[generator(yield(Board))]
        fn generate_moves(board: &Board, player: Player) {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in board.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if board[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    let mut next_board = board.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        }

        mk_gen!(let generator = box generate_moves(self, player));

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_old_next_gen(&self, player: Player) {
        use old_next_gen as next_gen;
        use old_next_gen::prelude::*;

        #[generator(Board)]
        fn generate_moves(board: &Board, player: Player) {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in board.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if board[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    let mut next_board = board.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        }

        mk_gen!(let generator = generate_moves(self, player));

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_old_next_gen_boxed(&self, player: Player) {
        use old_next_gen as next_gen;
        use old_next_gen::prelude::*;

        #[generator(Board)]
        fn generate_moves(board: &Board, player: Player) {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in board.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if board[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    let mut next_board = board.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        }

        mk_gen!(let generator = box generate_moves(self, player));

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_genawaiter_stack(&self, player: Player) {
        use genawaiter::{stack::let_gen, yield_};

        let_gen!(generator, {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        });

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_genawaiter_rc(&self, player: Player) {
        use genawaiter::{rc::gen, yield_};

        let generator = gen!({
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        });

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_genawaiter_sync(&self, player: Player) {
        use genawaiter::{sync::gen, yield_};

        let generator = gen!({
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yield_!(next_board);
                                }
                            }
                        }
                    }
                }
            }
        });

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_generator_local(&self, player: Player) {
        use generator::{done, Gn};

        let generator = Gn::new_scoped_local(|mut s| {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    s.yield_with(next_board);
                                }
                            }
                        }
                    }
                }
            }
            done!();
        });

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_generator(&self, player: Player) {
        use generator::{done, Gn};

        let generator = Gn::new_scoped(move |mut s| {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    s.yield_with(next_board);
                                }
                            }
                        }
                    }
                }
            }
            done!();
        });

        for next_board in generator {
            black_box(next_board);
        }
    }

    fn possible_regular_moves_corosensei(&self, player: Player) {
        use corosensei::{CoroutineResult, ScopedCoroutine};

        let mut generator = ScopedCoroutine::new(|yielder, _| {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    yielder.suspend(next_board);
                                }
                            }
                        }
                    }
                }
            }
        });

        while let CoroutineResult::Yield(next_board) = generator.resume(()) {
            black_box(next_board);
        }
    }

    async fn possible_regular_moves_gen_z(&self, player: Player) {
        use futures::stream::StreamExt;
        use gen_z::gen_z;

        let generator = gen_z(|mut z| async move {
            /* Iterate through all tiles. */
            for (orig_coords, tile) in self.iter_row_major() {
                /* Check if the tile is a splittable stack of this player. */
                if let Tile::Stack(p, stack_size) = tile {
                    if p == player && stack_size > 1 {
                        /* Iterate through all straight line directions. */
                        for dir_offset in NEIGHBOR_OFFSETS {
                            /* Move to a direction as far as there are empty tiles. */
                            let mut coords = orig_coords;
                            loop {
                                /* Coordinates for the next tile in the direction.
                                 * Hack: negative numbers cannot be added to a usize, so they are
                                 * converted into usize with underflow and then added with overflow.
                                 * Same as: let next_coords = coords + dir_offset */
                                let next_coords = (
                                    coords.0.wrapping_add(dir_offset.0 as usize),
                                    coords.1.wrapping_add(dir_offset.1 as usize),
                                );

                                /* If next tile is empty, move to that tile. */
                                if self[next_coords] == Tile::Empty {
                                    coords = next_coords;
                                } else {
                                    break;
                                }
                            }
                            /* Check if we actually found any empty tiles in the direction. */
                            if coords != orig_coords {
                                /* Iterate through all the ways to split the stack. */
                                for split in 1..stack_size {
                                    /* Create the next board. */
                                    let mut next_board = self.clone();
                                    next_board[coords] = Tile::Stack(player, split);
                                    next_board[orig_coords] =
                                        Tile::Stack(player, stack_size - split);

                                    z.send(next_board).await;
                                }
                            }
                        }
                    }
                }
            }
        });

        let mut generator = Box::pin(generator);

        while let Some(next_board) = generator.next().await {
            black_box(next_board);
        }
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    let input = "
       0  -6   0  -1  
    +3   0   0  +10 +3   0  
   0   0   0   0   0   0  
 0  -9   0   0       0   0  
       0   0   0   0   0   0  
     0   0   0   0  
"
    .trim_matches('\n');
    let board = Board::parse(input).unwrap();
    let player = Player::Min;

    let mut g = c.benchmark_group("generator-benchmark");
    g.sample_size(1000);

    g.bench_function("00 no generator, just loops", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_loops(black_box(player)))
    });
    g.bench_function("01 no generator, just iterators", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_iterator(black_box(player)))
    });
    g.bench_function("02 native rust generator (unstable)", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_native(black_box(player)))
    });
    g.bench_function("03 next-gen 0.1.1", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_next_gen(black_box(player)))
    });
    g.bench_function("04 next-gen 0.1.1 boxed", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_next_gen_boxed(black_box(player)))
    });
    g.bench_function("05 next-gen 0.0.10", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_old_next_gen(black_box(player)))
    });
    g.bench_function("06 next-gen 0.0.10 boxed", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_old_next_gen_boxed(black_box(player)))
    });
    g.bench_function("07 genawaiter stack", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_genawaiter_stack(black_box(player)))
    });
    g.bench_function("08 genawaiter rc", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_genawaiter_rc(black_box(player)))
    });
    g.bench_function("09 genawaiter sync", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_genawaiter_sync(black_box(player)))
    });
    g.bench_function("10 generator-rs local", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_generator_local(black_box(player)))
    });
    g.bench_function("11 generator-rs", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_generator(black_box(player)))
    });
    g.bench_function("12 corosensei", |b| {
        b.iter(|| black_box(&board).possible_regular_moves_corosensei(black_box(player)))
    });
    g.bench_function("13 gen-z", |b| {
        b.to_async(FuturesExecutor)
            .iter(|| black_box(&board).possible_regular_moves_gen_z(black_box(player)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
