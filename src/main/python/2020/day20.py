#!/usr/bin/env python3

import sys

import numpy as np


def orientations(matrix):
    for rotation in range(4):
        new_matrix = np.rot90(matrix, k=rotation)
        yield new_matrix
        yield np.flip(new_matrix, 1)


if __name__ == '__main__':
    tiles = {}
    for block in open(sys.argv[1]).read().split('\n\n'):
        lines = block.splitlines()
        tile_id = int(lines[0].split(' ')[1][:-1])
        tile = np.array([list(line) for line in lines[1:]]) == '#'
        tiles[tile_id] = tile

    grid_width = int(np.sqrt(len(tiles)))

    combined_tiles = []
    found_all_matches = False
    for start_tile_id in tiles.keys():
        for start_tile in orientations(tiles[start_tile_id]):
            combined_tiles = [(start_tile_id, start_tile)]
            used = {start_tile_id}
            found_all_matches = True
            for i in range(1, len(tiles)):
                found_match = False
                for tile_num, tile in tiles.items():
                    if tile_num in used:
                        continue

                    previous_horizontal = combined_tiles[i - 1][1] if i % grid_width != 0 else None
                    previous_vertical = combined_tiles[i - grid_width][1] if i // grid_width != 0 else None

                    for try_tile in orientations(tile):
                        if previous_horizontal is not None and not np.all(previous_horizontal[:, -1] == try_tile[:, 0]):
                            continue
                        if previous_vertical is not None and not np.all(previous_vertical[-1, :] == try_tile[0, :]):
                            continue

                        combined_tiles.append((tile_num, try_tile))
                        used.add(tile_num)
                        found_match = True
                        break

                if not found_match:
                    found_all_matches = False
                    break

            if found_all_matches:
                break

        if found_all_matches:
            break

    final_grid_ids = np.array([entry[0] for entry in combined_tiles]).reshape((grid_width, grid_width))
    corner_product = final_grid_ids[0, 0] * final_grid_ids[0, -1] * final_grid_ids[-1, 0] * final_grid_ids[-1, -1]
    print(f'Part 1: {corner_product}')

    tile_width = combined_tiles[0][1].shape[0] - 2  # Remove borders on each side
    grid = np.zeros((tile_width * grid_width, tile_width * grid_width), dtype=bool)
    for index, (_, tile) in enumerate(combined_tiles):
        row, column = divmod(index, grid_width)
        grid[
            row * tile_width:row * tile_width + tile_width,
            column * tile_width:column * tile_width + tile_width
        ] = tile[1:-1, 1:-1]

    monster = (
        '                  # ',
        '#    ##    ##    ###',
        ' #  #  #  #  #  #   '
    )
    monster = np.array([list(line) for line in monster]) == '#'

    water_roughness = None
    for orientation in orientations(grid):
        num_monsters = 0

        for i in range(grid.shape[0] - monster.shape[0] + 1):
            for j in range(grid.shape[1] - monster.shape[1] + 1):
                grid_slice = orientation[i:i + monster.shape[0], j:j + monster.shape[1]]
                if np.all((grid_slice & monster) == monster):
                    num_monsters += 1

        if num_monsters > 0:
            water_roughness = np.sum(grid) - num_monsters * np.sum(monster)
            break

    print(f'Part 2: {water_roughness}')
