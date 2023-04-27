
def check(array, numberOfBalls):
    numberList = array[:]
    count = 0
    while (count < 10):
        done = True
        for i in numberList:
            if i != numberOfBalls:
                done = False
                break
        
        if done:
            return True
        
        if (numberList[0] > numberList[1]):
            temp = numberList[0] - 1
            numberList[0] = numberList[1] + 1 
            numberList[1] = temp

        numberList.append(numberList[0])
        numberList.pop(0)

        count += 1
    return False

def filter(array):
    pass

# numberOfBalls = 3       
# good_sets = []
# for x in range(10):
#     for y in range(10):
#         for z in range(10):
#             if ((x + y + z) / 3 == (x + y + z) // 3 == numberOfBalls):
#                 if check([x,y,z], numberOfBalls):
#                     print(x, y, z)

numberOfBalls = 4       
good_sets = []
for x in range(10):
    for y in range(10):
        for z in range(10):        
            for i in range(10):
                if ((x + y + z+i) / 4 == (x + y + z+i) // 4 == numberOfBalls):
                    if check([x,y,z,i], numberOfBalls):
                        print(x, y, z, i)