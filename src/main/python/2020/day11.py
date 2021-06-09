import sys


def find_equilibrium(state, min_occupancy=4, level_1=True):
    max_row, max_col = len(state), len(state[0])
    directions = [
        (-1, 0),  # N
        (-1, 1),  # NE
        (0, 1),   # E
        (1, 1),   # SE
        (1, 0),   # S
        (1, -1),  # SW
        (0, -1),  # W
        (-1, -1)  # NW
    ]

    def occupied_neighbors(ferry, row, col):
        def neighbor(direction):
            new_row, new_col = row, col
            while True:
                new_row += direction[0]
                new_col += direction[1]
                if not (0 <= new_row < max_row and 0 <= new_col < max_col):
                    return 'out of bounds'
                else:
                    seat = ferry[new_row][new_col]
                    if seat in 'L#' or level_1:
                        return seat

        return sum(neighbor(direction) == '#' for direction in directions)

    old_state = []
    while old_state != state:
        old_state = state
        state = []
        for row in range(max_row):
            new_row = ''
            for col in range(max_col):
                current_seat = old_state[row][col]
                occ_neighbors = occupied_neighbors(old_state, row, col)
                if current_seat == 'L' and occ_neighbors == 0:
                    new_row += '#'
                elif current_seat == '#' and occ_neighbors >= min_occupancy:
                    new_row += 'L'
                else:
                    new_row += current_seat

            state.append(new_row)

    return sum(row.count('#') for row in state)


if __name__ == '__main__':
    with open(sys.argv[1]) as file:
        seats = file.read().strip().split('\n')

    print(f'Part 1: {find_equilibrium(seats)}')
    print(f'Part 2: {find_equilibrium(seats, min_occupancy=5, level_1=False)}')
