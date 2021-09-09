import subprocess
import shutil
import sys
import os

# Utilities

def fail(message, errcode=1):
  print(f'[-] {message} (code = {errcode})')
  sys.exit(errcode)

def execute(command, must_succeed=True, environment=None):
  rc = 0
  try:
    p = subprocess.Popen(command, shell=True, env=environment)
    p.communicate()
    rc = p.returncode
  except subprocess.CalledProcessError as ex:
    fail(f'Failure while executing: {command}', rc)
  if must_succeed and rc:
    fail(f'Failure while executing: {command}', rc)

def find_and_copy(_src, _dir, _dst):
  # Find the '_src' file/folder in the '_dir' folder
  for root, dirs, files in os.walk(_dir):
    # Search the directories
    for dirn in dirs:
      print(dirn)
      if dirn == _src:
        # Full source path
        path = os.path.join(root, dirn)
        # Delete the folder if it exists
        if os.path.isdir(_dst):
          os.rmdir(_dst)
        # Copy it to the '_dst' folder
        shutil.copytree(path, _dst)
        # Notify we found the folder
        return True
    # Search the files
    for file in files:
      if file == _src:
        # Full source path
        path = os.path.join(root, file)
        # Delete the file if it exists
        if os.path.isfile(_dst):
          os.remove(_dst)
        # Copy it to the '_dst' folder
        shutil.copyfile(path, _dst)
        # Notify we found the file
        return True
  # Notify we didn't find the file
  return False

# LLVM setup

LLVM_GIT='https://github.com/llvm/llvm-project'
LLVM_TAG='release/13.x'

def setupLLVM(cwd):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'llvm')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone -b {LLVM_TAG} {LLVM_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      '-DCMAKE_C_COMPILER=clang',
      '-DCMAKE_CXX_COMPILER=clang++',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '-DLLVM_TARGETS_TO_BUILD="X86;AArch64"',
      '-DLLVM_BUILD_TESTS=False',
      '-DLLVM_INCLUDE_TESTS=False',
      '-DLLVM_INCLUDE_BENCHMARKS=False',
      '-DLLVM_ENABLE_ASSERTIONS=True',
      '-DLLVM_BUILD_DOCS=False',
      '-DLLVM_ENABLE_DOXYGEN=False',
      '-DLLVM_ENABLE_DUMP=True',
      '-DLLVM_ENABLE_RTTI=True',
      '-DLIBCXX_ENABLE_SHARED=YES',
      '-DLIBCXX_ENABLE_STATIC=NO',
      '-DLIBCXX_ENABLE_EXPERIMENTAL_LIBRARY=NO',
      '-DLIBCXX_INSTALL_LIBRARY=ON',
      '-DLIBCXX_INSTALL_HEADERS=ON',
      '-DLLVM_ENABLE_PROJECTS="clang;libcxx;libcxxabi"',
      '../llvm'
      )))
    execute('ninja')
    execute('ninja install')
  # Return the installation directory
  return install_dir

# KLEE setup

KLEE_GIT='https://github.com/fvrmatteo/klee'
KLEE_TAG='pure-bv-qf-llvm-7.0'

def setupKLEE(cwd):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'klee')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone -b {KLEE_TAG} {KLEE_GIT} {clone_dir}')
  # Return the cloned directory
  return clone_dir

# CAPSTONE setup

CAPSTONE_GIT='https://github.com/aquynh/capstone'

def setupCAPSTONE(cwd):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'capstone')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone {CAPSTONE_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      '-DCMAKE_C_COMPILER=clang',
      '-DCMAKE_CXX_COMPILER=clang++',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '-DCAPSTONE_BUILD_CSTOOL=no',
      '-DCAPSTONE_BUILD_TESTS=no',
      '-DCAPSTONE_TMS320C64X_SUPPORT=0',
      '-DCAPSTONE_MOS65XX_SUPPORT=0',
      '-DCAPSTONE_SPARC_SUPPORT=0',
      '-DCAPSTONE_M680X_SUPPORT=0',
      '-DCAPSTONE_XCORE_SUPPORT=0',
      '-DCAPSTONE_M68K_SUPPORT=0',
      '-DCAPSTONE_MIPS_SUPPORT=0',
      '-DCAPSTONE_SYSZ_SUPPORT=0',
      '-DCAPSTONE_PPC_SUPPORT=0',
      '-DCAPSTONE_ARM_SUPPORT=0',
      '-DCAPSTONE_EVM_SUPPORT=0',
      '-DCAPSTONE_ARM64_SUPPORT=1',
      '-DCAPSTONE_X86_SUPPORT=1',
      '..'
      )))
    execute('ninja')
    execute('ninja install')
  # Return the installation directory
  return install_dir

# Z3 setup

Z3_GIT='https://github.com/Z3Prover/z3'

def setupZ3(cwd):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'z3')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone {Z3_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      '-DCMAKE_C_COMPILER=clang',
      '-DCMAKE_CXX_COMPILER=clang++',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '-DZ3_BUILD_LIBZ3_SHARED=FALSE',
      '..'
      )))
    execute('ninja')
    execute('ninja install')
  # Return the installation directory
  return install_dir

# HIREDIS setup

HIREDIS_GIT='https://github.com/redis/hiredis'

def setupHIREDIS(cwd):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'hiredis')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone {HIREDIS_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      '-DCMAKE_C_COMPILER=clang',
      '-DCMAKE_CXX_COMPILER=clang++',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '..'
      )))
    execute('ninja')
    execute('ninja install')
  # Return the installation directory
  return install_dir

# ALIVE2 setup

ALIVE2_GIT='https://github.com/pgarba/alive2'
ALIVE2_TAG='saturn_patches'

def setupALIVE2(cwd, z3_dir):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'alive2')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone -b {ALIVE2_TAG} {ALIVE2_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      f"-DZ3_INCLUDE_DIR={os.path.join(z3_dir, 'include')}",
      '-DCMAKE_C_COMPILER=clang',
      '-DCMAKE_CXX_COMPILER=clang++',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '..'
      )))
    execute('ninja')
    # Create the folders
    include_dir = os.path.join(install_dir, 'include', 'alive2')
    lib_dir = os.path.join(install_dir, 'lib')
    if not os.path.isdir(include_dir):
      os.makedirs(include_dir)
    if not os.path.isdir(lib_dir):
      os.mkdir(lib_dir)
    # Copy the static libraries
    if not find_and_copy('libir.a', '.', f'{lib_dir}/libir.a'):
      fail('Failed to find and copy file: libir.a')
    if not find_and_copy('libsmt.a', '.', f'{lib_dir}/libsmt.a'):
      fail('Failed to find and copy file: libsmt.a')
    if not find_and_copy('libutil.a', '.', f'{lib_dir}/libutil.a'):
      fail('Failed to find and copy file: libutil.a')
    if not find_and_copy('libtools.a', '.', f'{lib_dir}/libtools.a'):
      fail('Failed to find and copy file: libtools.a')
    # Copy the includes
    if not find_and_copy('ir', '..', f'{include_dir}/ir'):
      fail('Failed to find and copy folder: ir')
    if not find_and_copy('smt', '..', f'{include_dir}/smt'):
      fail('Failed to find and copy folder: smt')
    if not find_and_copy('tools', '..', f'{include_dir}/tools'):
      fail('Failed to find and copy folder: tools')
    if not find_and_copy('util', '..', f'{include_dir}/util'):
      fail('Failed to find and copy folder: util')
  # Return the installation directory
  return install_dir

# REMILL setup

REMILL_GIT='https://github.com/lifting-bits/remill'

def setupREMILL(cwd):
  # Determine the directory
  clone_dir = os.path.join(cwd, 'remill')
  # Skip cloning if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone {REMILL_GIT} {clone_dir}')

# TRITON setup

TRITON_GIT='https://github.com/JonathanSalwan/Triton'

def setupTRITON(cwd, capstone_dir, boost_dir):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'triton')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone {TRITON_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      f"-DCAPSTONE_INCLUDE_DIR={os.path.join(capstone_dir, 'include')}",
      f"-DCAPSTONE_LIBRARY={os.path.join(capstone_dir, 'lib', 'libcapstone.a')}",
      f"-DBoost_INCLUDE_DIR={boost_dir}",
      '-DCMAKE_C_COMPILER=clang',
      '-DCMAKE_CXX_COMPILER=clang++',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '-DPYTHON_BINDINGS=OFF',
      '-DZ3_INTERFACE=OFF',
      '-DSTATICLIB=ON',
      '-DKERNEL4=OFF',
      '..'
      )))
    execute('ninja')
    execute('ninja install')

# SOUPER setup

SOUPER_GIT='https://github.com/pgarba/souper'
SOUPER_TAG='saturn_patches_z3_native_llvm13'

def setupSOUPER(cwd, hiredis_dir, alive2_dir, klee_dir, llvm_dir, z3_dir):
  # Determine the directories
  clone_dir = os.path.join(cwd, 'souper')
  build_dir = os.path.join(clone_dir, 'build')
  install_dir = os.path.join(clone_dir, 'install')
  # Skip compilation if needed
  if not os.path.isdir(clone_dir):
    # Clone sources
    execute(f'git clone -b {SOUPER_TAG} {SOUPER_GIT} {clone_dir}')
    # Create the build and install directory
    if not os.path.isdir(build_dir):
      os.mkdir(build_dir)
    if not os.path.isdir(install_dir):
      os.mkdir(install_dir)
    os.chdir(build_dir)
    # Compile sources
    execute(' '.join((
      'cmake -G Ninja',
      f'-DCMAKE_INSTALL_PREFIX={install_dir}',
      f"-DCMAKE_C_COMPILER={os.path.join(llvm_dir, 'bin', 'clang')}",
      f"-DCMAKE_CXX_COMPILER={os.path.join(llvm_dir, 'bin', 'clang++')}",
      f'-DHIREDIS_FOLDER={hiredis_dir}',
      f'-DALIVE2_FOLDER={alive2_dir}',
      f'-DKLEE_FOLDER={klee_dir}',
      f'-DLLVM_FOLDER={llvm_dir}',
      f'-DZ3_FOLDER={z3_dir}',
      '-DCMAKE_C_FLAGS="-march=native -O3"',
      '-DCMAKE_CXX_FLAGS="-march=native -O3"',
      '-DCMAKE_BUILD_TYPE=Release',
      '-DBUILD_CLANG_TOOL=0',
      '-DBUILD_DOCUMENTATION=0',
      '-DTEST_LONG_DURATION_SYNTHESIS=0',
      '-DTEST_SYNTHESIS=0',
      '-DGO_EXECUTABLE="GO_EXECUTABLE-NOTFOUND"',
      '..'
      )))
    execute('ninja')
    # Create the folders
    include_dir = os.path.join(install_dir, 'include')
    lib_dir = os.path.join(install_dir, 'lib')
    if not os.path.isdir(include_dir):
      os.makedirs(include_dir)
    if not os.path.isdir(lib_dir):
      os.mkdir(lib_dir)
    # Copy the static libraries
    if not find_and_copy('libkleeExpr.a', '.', f'{lib_dir}/libkleeExpr.a'):
      fail('Failed to find and copy file: libkleeExpr.a')
    if not find_and_copy('libprofileRuntime.a', '.', f'{lib_dir}/libprofileRuntime.a'):
      fail('Failed to find and copy file: libprofileRuntime.a')
    if not find_and_copy('libsouperCodegen.a', '.', f'{lib_dir}/libsouperCodegen.a'):
      fail('Failed to find and copy file: libsouperCodegen.a')
    if not find_and_copy('libsouperCodegenStatic.a', '.', f'{lib_dir}/libsouperCodegenStatic.a'):
      fail('Failed to find and copy file: libsouperCodegenStatic.a')
    if not find_and_copy('libsouperExtractor.a', '.', f'{lib_dir}/libsouperExtractor.a'):
      fail('Failed to find and copy file: libsouperExtractor.a')
    if not find_and_copy('libsouperInfer.a', '.', f'{lib_dir}/libsouperInfer.a'):
      fail('Failed to find and copy file: libsouperInfer.a')
    if not find_and_copy('libsouperInst.a', '.', f'{lib_dir}/libsouperInst.a'):
      fail('Failed to find and copy file: libsouperInst.a')
    if not find_and_copy('libsouperKVStore.a', '.', f'{lib_dir}/libsouperKVStore.a'):
      fail('Failed to find and copy file: libsouperKVStore.a')
    if not find_and_copy('libsouperParser.a', '.', f'{lib_dir}/libsouperParser.a'):
      fail('Failed to find and copy file: libsouperParser.a')
    if not find_and_copy('libsouperPassStatic.a', '.', f'{lib_dir}/libsouperPassStatic.a'):
      fail('Failed to find and copy file: libsouperPassStatic.a')
    if not find_and_copy('libsouperSMTLIB2.a', '.', f'{lib_dir}/libsouperSMTLIB2.a'):
      fail('Failed to find and copy file: libsouperSMTLIB2.a')
    if not find_and_copy('libsouperTool.a', '.', f'{lib_dir}/libsouperTool.a'):
      fail('Failed to find and copy file: libsouperTool.a')
    # Copy the includes
    if not find_and_copy('souper', '../include', f'{include_dir}/souper'):
      fail('Failed to find and copy folder: souper')
    if not find_and_copy('klee', '../include', f'{include_dir}/klee'):
      fail('Failed to find and copy folder: klee')

# BOOST setup

BOOST_URL='https://versaweb.dl.sourceforge.net/project/boost/boost/1.68.0/boost_1_68_0.7z'
BOOST_TAG='boost_1_68_0'

def setupBOOST(cwd):
  # Determine the directory
  clone_dir = os.path.join(cwd, 'boost')
  # Skip cloning if needed
  if not os.path.isdir(clone_dir):
    # Create the clone directory
    if not os.path.isdir(clone_dir):
      os.mkdir(clone_dir)
    os.chdir(clone_dir)
    # Clone sources
    execute(f'curl -O {BOOST_URL}')
    # Extract the archive
    execute(f'7z x {BOOST_TAG}.7z')
  # Return the downloded directory
  return os.path.join(clone_dir, BOOST_TAG)

# Requirements

def setupRequirements():
  # Install 'git'
  execute('sudo apt install git')
  # Install 'cmake'
  execute('sudo apt install cmake')
  # Install 'ninja'
  execute('sudo apt install ninja')
  # Install 'python'
  execute('sudo apt install python3')
  # Install 'clang'
  execute('sudo apt install clang-12')
  # Install 'p7zip'
  execute('sudo apt install p7zip-full')
  # Install 'build-essential'
  execute('sudo apt install build-essential')
  # Install 'boost'
  execute('sudo apt install libboost1.71-all-dev')

# Entry point

if __name__ == '__main__':
  # Detect the current directory
  cwd = os.getcwd()
  # setupRequirements()
  boost_dir = setupBOOST(cwd)
  setupREMILL(cwd)
  capstone_dir = setupCAPSTONE(cwd)
  setupTRITON(cwd, capstone_dir, boost_dir)
  z3_dir = setupZ3(cwd)
  llvm_dir = setupLLVM(cwd)
  klee_dir = setupKLEE(cwd)
  hiredis_dir = setupHIREDIS(cwd)
  alive2_dir = setupALIVE2(cwd, z3_dir)
  setupSOUPER(cwd, hiredis_dir, alive2_dir, klee_dir, llvm_dir, z3_dir)
