import subprocess
import random
import sys

CORRECT = r"c:\Users\jwoo\Desktop\vscode_github\BigIntergreter\ntt_3_ans.exe"
TWO = r"c:\Users\jwoo\Desktop\vscode_github\BigIntergreter\ntt_faster_two.exe"
THREE = r"c:\Users\jwoo\Desktop\vscode_github\BigIntergreter\ntt_faster_three.exe"


def run(exe, a, b):
    inp = f"{a} {b}\n".encode()

    p = subprocess.run(
        exe,
        input=inp,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )

    out = p.stdout.decode(errors="ignore").strip().split()

    # 마지막 줄이 정답
    return out[-1]


for _ in range(100):
    la = random.randint(1, 17)
    lb = random.randint(1, 17)

    a = str(random.randint(10**(la-1), 10**la-1))
    b = str(random.randint(10**(lb-1), 10**lb-1))
#    a=abs(int("9"*random.randint(1, 30))-random.randint(0, 10**(random.randint(1, 7))-1))
#    b=abs(int("9"*random.randint(1, 30))-random.randint(0, 10**(random.randint(1, 7))-1))
    a=b="9"*random.randint(19000,21000)
    try:
        ans = run(CORRECT, a, b)
        print("ans")
        out2 = run(TWO, a, b)
        print("out2")
        out3 = run(THREE, a, b)
        print("out3")
    except Exception as e:
        print("Error occurred!")
        print("input:",f"{a} {b}")
        print("Error:", e)
        break

    if ans != out2 or ans != out3:
        print("Mismatch!")
        print("A =", a)
        print("B =", b)
        print("Correct    =", ans)
        print("Two        =", out2)
        print("Three      =", out3)
        # print("python ans =", str(int(a) * int(b)))
        break
print("All tests passed!")