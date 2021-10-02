import random

def game(changemode,choosewhat,doorlength):
    door = [False] * doorlength
    answer_idx = random.randint(0, len(door)-1)
    door[answer_idx] = True

    # print("Input 0...{}".format(len(door)-1))
    chooseidx = choosewhat
    # print("change? y/n")
    change = changemode

    if change == "y" or change == "Y":
        chooseidx = closed_door_idx(answer_idx, chooseidx, door)
    if door[chooseidx]:
        # print("You win!")
        return True
    else:
        # print("You lose!")
        return False

def closed_door_idx(ans,chs,door)->int:
    retlist = range(len(door))
    if ans == chs:
        return random.choice(list(filter(lambda x: x != ans, retlist)))
    else:
        return ans

def main():
    win = lose = 0
    do_change = "y" # "y" or "n"
    choose_index = 0
    door_count = 3
    for _ in range(10000):
        if game(do_change,choose_index,door_count):
            win = win + 1
        else:
           lose = lose + 1
    print("win: {}, lose: {}".format(win,lose))

if __name__ == "__main__":
    main()