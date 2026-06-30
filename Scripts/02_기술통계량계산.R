# 1. 데이터 불러오기
file_path <- "Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv"
df <- read.csv(file_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 2. 가마크기 필터링 함수 정의 및 적용
# 가마크기_길이 또는 가마크기_폭의 값이 '?', '-', 괄호 포함, 또는 빈칸인 행 제외
is_valid <- function(x) {
  !is.na(x) & x != "" & x != "?" & x != "-" & !grepl("(", x, fixed = TRUE) & !grepl(")", x, fixed = TRUE)
}

# subset 명령어를 우선적으로 사용하여 데이터 필터링
df_clean <- subset(df, is_valid(가마크기_길이) & is_valid(가마크기_폭))

# 가마크기 값을 수치형(Numeric)으로 변환
df_clean$가마크기_길이 <- as.numeric(df_clean$가마크기_길이)
df_clean$가마크기_폭 <- as.numeric(df_clean$가마크기_폭)

# 3. 재임방식 변환
# 재임방식이 '='이거나 '?'인 경우 '알 수 없음'으로 변환
# (실제 데이터의 '-' 및 빈값 ""도 결측치를 의미하므로 함께 '알 수 없음'으로 처리)
# '무시설식'은 그대로 유지, 그 외(할석, 할석+도침, 홈+할석, 홈내기식 등)는 '유시설식'으로 변환
df_clean$연소부_재임방식 <- ifelse(df_clean$연소부_재임방식 == "=" | 
                                 df_clean$연소부_재임방식 == "?" | 
                                 df_clean$연소부_재임방식 == "-" | 
                                 df_clean$연소부_재임방식 == "", "알 수 없음",
                               ifelse(df_clean$연소부_재임방식 == "무시설식", "무시설식", "유시설식"))

# ==========================================
# 대옹 출토 여부에 따른 데이터 분할
# ==========================================
# 대옹_출토 열에 '●'로 표기된 것들만 출토된 것, 나머지는 출토되지 않은 것
df_pot_yes <- subset(df_clean, !is.na(대옹_출토) & 대옹_출토 == "●")
df_pot_no  <- subset(df_clean, is.na(대옹_출토) | 대옹_출토 != "●")

# ==========================================
# summary() 함수를 활용한 기술통계량 계산 (Text 출력)
# ==========================================

# Results 폴더가 없을 경우 생성
if (!dir.exists("Results")) {
  dir.create("Results")
}

# 텍스트 파일로 출력 저장 시작 (sink 활용)
output_file <- "Results/가마크기_기술통계량.txt"
sink(output_file)

cat("==================================================\n")
cat("가마크기 기술통계량 요약 결과 (summary 함수 사용)\n")
cat("==================================================\n\n")

cat("[1] 전체 데이터 기술통계량 (N =", nrow(df_clean), ")\n")
cat("--------------------------------------------------\n")
cat("1. 가마크기_길이 요약:\n")
print(summary(df_clean$가마크기_길이))
cat("\n2. 가마크기_폭 요약:\n")
print(summary(df_clean$가마크기_폭))
cat("\n\n")

cat("[2] 대옹 출토 여부별 기술통계량\n")
cat("--------------------------------------------------\n")

cat("■ 대옹 출토 가마 (N =", nrow(df_pot_yes), ")\n")
cat("1. 가마크기_길이 요약:\n")
print(summary(df_pot_yes$가마크기_길이))
cat("\n2. 가마크기_폭 요약:\n")
print(summary(df_pot_yes$가마크기_폭))
cat("\n")

cat("■ 대옹 미출토 가마 (N =", nrow(df_pot_no), ")\n")
cat("1. 가마크기_길이 요약:\n")
print(summary(df_pot_no$가마크기_길이))
cat("\n2. 가마크기_폭 요약:\n")
print(summary(df_pot_no$가마크기_폭))
cat("\n")

sink()
cat("기술통계량 요약 텍스트 파일 저장 완료:", output_file, "\n")
