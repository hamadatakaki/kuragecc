import subprocess


def generate_example_case(kind, index):
    for i in index:
        subprocess.run(f"touch example/{kind}{i}.tmpc", shell=True)
