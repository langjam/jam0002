import os
import sys
import subprocess

def clone_repo():
     os.system(f'git clone --recursive https://github.com/Deruago/theDeamerProject')
     
def install_repo():
    os.system(f'cd theDeamerProject && python3 installer.py')

if __name__ == "__main__":
    clone_repo()
    install_repo()