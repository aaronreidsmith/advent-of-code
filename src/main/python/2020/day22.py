import sys


def calculate_score(deck):
    return sum([(idx + 1) * value for idx, value in enumerate(deck[::-1])])


def play_game(player1, player2):
    seen = set()

    while player1 and player2:
        key = (tuple(player1), tuple(player2))
        if key in seen:
            return player1, []
        else:
            seen.add(key)

        p1_card, player1 = player1[0], player1[1:]
        p2_card, player2 = player2[0], player2[1:]
        if p1_card <= len(player1) and p2_card <= len(player2):
            player1_prime = player1[:p1_card]
            player2_prime = player2[:p2_card]
            player1_prime, player2_prime = play_game(player1_prime, player2_prime)
            if len(player1_prime) > len(player2_prime):
                player1.extend([p1_card, p2_card])
            else:
                player2.extend([p2_card, p1_card])
        elif p1_card > p2_card:
            player1.extend([p1_card, p2_card])
        else:
            player2.extend([p2_card, p1_card])

    return player1, player2


if __name__ == '__main__':
    with open(sys.argv[1]) as file:
        p1_string, p2_string = file.read().split('\n\n')
        player1 = [int(line) for line in p1_string.split('\n')[1:]]
        player2 = [int(line) for line in p2_string.split('\n')[1:]]

    p1, p2 = play_game(player1, player2)
    print(calculate_score(p1 + p2))
