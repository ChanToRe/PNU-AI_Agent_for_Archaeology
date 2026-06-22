# AI 코딩을 활용한 고고학 자료 분석 실습

본 레포지토리는 2026년 7월 15일 부산대학교 고고학과 BK21 사업단 특강 `AI 코딩을 활용한 고고학 자료 분석 실습`의 강의자료를 정리해둔 레포지토리입니다. 




## 1. 개발환경 구성

### 1.1 R

#### 소개

R은 데이터 분석 및 통계분석에 특화된 오픈소스 프로그래밍 언어입니다. University of Auckland의 Ross Ihaka와 Robert Gentleman에 의해 개발되었으며, 데이터 처리와 시각화에 특화되어 있습니다. 통게분석과 관련된 수많은 패키지들이 구현되어 있으며, 고고학 분석에 활용되는 패키지들도 대부분 R로 구현되어 있습니다.

#### 설치

1. CRAN 접속: https://cran.r-project.org/에 접속합니다.
2. 운영체제 선택: 화면 상단의 `Download R for (운영체제)` 링크를 클릭합니다.
3. 설치 파일 다운로드:
   1. MacOS: 자신의 프로세서에 맞는 버전을 선택합니다.
       - Apple Silicon (M1, M2, M3 등): `arm64.pkg` 선택
       - Intel: `x86.pkg` 선택
   2. Windows: base를 클릭한 후, 화면 최상단의 `Download R-X.X.X for Windows`를 클릭합니다.
4. 설치 진행: 다운로드된 설치 파일을 실행하여 기본 설정 그대로 설치를 완료합니다.

### 1.2 IDE

#### 소개
통합개발환경(Integrated Development Environment, IDE)는 코드를 작성, 편집, 컴파일 및 디버그할 수 있는 소프트웨어입니다. 이를 통해 코드 작성하고 프로젝트를 직관적으로 관리할 수 있습니다. 대표적인 IDE로는 Microsoft의 `VScode`, `Visual Studio` / Jetbrains의 `IntelliJ IDEA`, `Pycharms` Apple의 `Xcode` 등이 있습니다.

#### 설치
본 강의에서는 가장 범용적으로 사용되는 IDE인 VSCode를 활용합니다. 
1. 공식 홈페이지 접속: https://code.visualstudio.com/에 접속합니다.
2. 설치 파일 다운로드: 메인 화면의 파란색 `Download for (운영체제)` 버튼을 클릭합니다.
3. 설치 진행: 다운로드된 Setup 파일을 실행하여 설치를 완료합니다.

### 1.3 LLM TUI 설치

#### Claude Code
`Command(⌘) + Spacebar`를 눌러 
터미널 검색 후 실행
##### - MacOS & Linux
Mac 또는 Linux에서 `Claude Code`를 설치하려면 아래의 명령을 실행하세요.
``` bash
curl -fsSL https://claude.ai/install.sh | bash
```

##### - Windows
Windows에서 Claude Code를 설치하려면 아래의 명령을 실행하세요.
```
irm https://claude.ai/install.ps1 | iex
```
#### Antigravity CLI
##### - MacOS & Linux
Mac 또는 Linux에서 `Antigravity CLI`를 설치하려면 아래의 명령을 실행하세요.x
``` bash
curl -fsSL https://antigravity.google/cli/install.sh | bash
```

##### - Windows
``` bash
irm https://antigravity.google/cli/install.ps1 | iex
```

#### Codex CLI
##### - MacOS & Linux
Mac 또는 Linux에서 `Codex CLI`를 설치하려면 아래의 명령을 실행하세요.
``` bash
curl -fsSL https://chatgpt.com/codex/install.sh | sh
```

##### - Windows
``` powershell 
irm https://chatgpt.com/codex/install.ps1 | iex
```

