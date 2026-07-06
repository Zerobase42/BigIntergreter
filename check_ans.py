import subprocess
import random

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

    out = p.stdout.decode(errors="ignore").strip().splitlines()

    # 마지막 줄이 정답
    return out[-1]


while True:
    la = random.randint(1, 8)
    lb = random.randint(1, 8)

    a = str(random.randint(10**(la-1), 10**la-1))
    b = str(random.randint(10**(lb-1), 10**lb-1))

    ans = run(CORRECT, a, b)
    out2 = run(TWO, a, b)
    out3 = run(THREE, a, b)

    if ans != out2 or ans != out3:
        print("Mismatch!")
        print("A =", a)
        print("B =", b)
        print("Correct =", ans)
        print("Two     =", out2)
        print("Three   =", out3)
        print("python ans =", str(int(a) * int(b)))
        break