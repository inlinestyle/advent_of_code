
angles = {'L': 1j, 'R': -1j}

def add(acc, direction):
    acc['dim'] *= angles[direction[0]]
    prev_point = acc['point']
    length = int(direction[1:])
    acc['point'] += acc['dim'] * length
    for i in range(length):
        intermediate_point = prev_point + acc['dim'] * (i + 1)
        if intermediate_point in acc['visited']:
            print 'revisited: {}'.format(intermediate_point)
        acc['visited'].add(intermediate_point)

    return acc
    

def process_directions(directions):
    return reduce(add, directions, {'point': 0, 'dim': 1j, 'visited': {0j}})['point']
        

if __name__ == '__main__':
    lines = []
    with open('dave_input.txt', 'r') as f:
        for line in f:
            lines.append(line.strip('\n').split(', '))
    for line in lines:
        point = process_directions(line)
        print 'endpoint: {}'.format(point)

