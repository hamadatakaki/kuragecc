import subprocess
from subprocess import PIPE

class TmpFileManager:
    def __init__(self, dirs, files):
        self.dirs = dirs
        self.files = files

    def __enter__(self):
        for d in self.dirs:
            subprocess.run(f"mkdir {d}", shell=True)

        for f in self.files:
            subprocess.run(f"touch {f}", shell=True)

    def __exit__(self, ex_type, ex_value, trace):
        for f in self.files:
            subprocess.run(f"rm {f}", shell=True)

        for d in self.dirs:
            subprocess.run(f"rm -r {d}", shell=True)

if __name__=='__main__':
    proc = subprocess.run("cargo run", shell=True, stdout=PIPE)

    with TmpFileManager(['tmp'], ['tmp/test.ll', "tmp/main.out"]):
        with open('tmp/test.ll', 'wb') as wb:
            wb.write(proc.stdout)
        
        subprocess.run("clang tmp/test.ll -o tmp/main.out", shell=True)
        proc = subprocess.run("tmp/main.out",shell=True, stdout=PIPE, stderr=PIPE)
        print(f"status code: {proc.returncode}")

