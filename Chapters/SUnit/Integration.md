## Getting more with continuous integrationOnce you have tests you want to run them as many times as possible. In this chapter we show the basic setup to execute your tests automatically on each commit. We present Github actions, and Travis integration both using SmalltalkCI to execute your tests on the server.### Github ActionsIf you use github to store your Pharo code, then you can use a nice continuous integration: GitHub actions.You can edit your script by pressing the Actions button of your github project. You can follow the template. It will create a `.github/workflows` folder on your project, and store your scripts. You can have multiple scripts. In addition you should add a file to configure SmalltalkCI. This file is in STON format \(a.k.a JSON for Smalltalk\). SmalltalkCI configuration lets you specify:- how to load your project \(`loading:`\)- which entities \(classes, packages, projects...\) to include in the tests \(`testing:` and `#include:`\)- which entities \(classes, packages, projects...\) to exclude from the tests \(`testing:` and `#exclude:`\).- several options such as time out, - but also test coverage \(`testing:` and `#coverage:`\)There is a good documentation of SmalltalkCI at [https://github.com/hpi-swa/smalltalkCI](https://github.com/hpi-swa/smalltalkCI).#### ExampleBut let us look at an example. Let us take for example the project Containers-Array2d from [http://github.com/pharo-containers](http://github.com/pharo-containers).You should first define a file named  `.smalltalk.ston` to indicate SmallttalkCI \(the application  that will run your tests\)how to identify them. Here we indicate how SmalltalkCI should find the source code and which packages should be executed by the tests in the contextof also computing the test coverage of that package. ```SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'ContainersArray2D',
      #directory : 'src',
      #platforms : [ #pharo ]
    }
  ],
  #testing : {
    #coverage : {
      #packages : [ 'Containers-Array2D' ]
    }
  }
}```Here is the currentStablePharo file.```name: currentStablePharo

env:
  GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}

on:
  push:
    branches:
      - master
  workflow_dispatch:
  
jobs:
  build:
    strategy:
      matrix:
        platform: [ubuntu-latest, macos-latest, windows-latest ]
    runs-on: ${{ matrix.platform }}
    steps:
      - uses: actions/checkout@v2
      - uses: hpi-swa/setup-smalltalkCI@v1
        id: smalltalkci
        with:
          smalltalk-version: Pharo64-8.0
      - run: smalltalkci -s ${{ steps.smalltalkci.outputs.smalltalk-version }}
        shell: bash
        timeout-minutes: 15```You can also use matrix setup to run multiple configuration of platforms: here we run on Pharo 8 and Pharo 7 for mac and windows.```name: matrix

on:
  push:
    branches: [ master ]
  pull_request:
    branches: [ master ]

  # Allows you to run this workflow manually from the Actions tab
  workflow_dispatch:

# A workflow run is made up of one or more jobs that can run sequentially or in parallel
jobs:
  build:
    strategy:
      matrix:
        os: [ macos-latest, windows-latest ]
        smalltalk: [ Pharo64-8.0, Pharo64-7.0 ]
    runs-on: ${{ matrix.os }}
    name: ${{ matrix.smalltalk }} on ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v2
      - name: Setup smalltalkCI
        uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-version: ${{ matrix.smalltalk }}
      - name: Load Image and Run Tests
        run: smalltalkci -s ${{ matrix.smalltalk }}
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        timeout-minutes: 15```### Travis	First you should enable the travis services on your github account. We sketch here the process.Better refer to the documentation since the process was changed in the past.Nowadays travis does not look to work as it was used to.It usually means that: - you should have an account on travis-ci.com \(travis-ci.org has been deprecated\).- you should get a token from Github `GH_Token` that you add to your travis account.- you have to encrypt \(using `travis encrypt key`\) your repository with the one time shown github key. Then you should add configuration files, for linux and mac a `.travis.yml` file and  windows a `appveyor.yml` one.### With TravisHere is a configuration file for travis. It should be named `.travis.yml`.```language: smalltalk
sudo: false

# Select operating system(s)
os:
  - linux
  - osx

# Select compatible Smalltalk image(s)
smalltalk:

  - Pharo32-8.0
  - Pharo-7.0
  - Pharo-6.1

  - Pharo64-8.0
  - Pharo64-7.0```### With Appveyor If you want to validate your tests on Windows, you  should create the file `appveyor.yml` file. Here is an example:```environment:
  CYG_ROOT: C:\cygwin
  CYG_BASH: C:\cygwin\bin\bash
  CYG_CACHE: C:\cygwin\var\cache\setup
  CYG_EXE: C:\cygwin\setup-x86.exe
  CYG_MIRROR: http://cygwin.mirror.constant.com
  SCI_RUN: /cygdrive/c/smalltalkCI-master/run.sh
  matrix:
    - SMALLTALK: Pharo-6.1
    - SMALLTALK: Pharo-7.0

platform:
  - x86

install:
  - '%CYG_EXE% -dgnqNO -R "%CYG_ROOT%" -s "%CYG_MIRROR%" -l "%CYG_CACHE%" -P unzip'
  - ps: Start-FileDownload "https://github.com/hpi-swa/smalltalkCI/archive/master.zip" "C:\smalltalkCI.zip"
  - 7z x C:\smalltalkCI.zip -oC:\ -y > NULL

build: false

test_script:
  - '%CYG_BASH% -lc "cd $APPVEYOR_BUILD_FOLDER; exec 0</dev/null; $SCI_RUN"'```### Assessing test coverageFinally once you have automated servers up and running, you can take advantage of other services such as computing the code coverage of your tests.SmalltalkCI is ready for this and you can simply use [http://coveralls.io](http://coveralls.io). First create an account to [http://www.coveralls.io](http://www.coveralls.io) and enable the communication between coverall and the project you want to cover. The previous smalltalkCI configuration you showed at the beginning of this chapter is already ready to be used with Coverall.### ConclusionRunning automatically your tests is a super great feeling. Do not miss this opportunity. This chapter shows you how easy it is to do it with github or travis.Have a look at the documentation of SmalltalkCI [https://github.com/hpi-swa/smalltalkCI](https://github.com/hpi-swa/smalltalkCI) to see what you do automatically. 