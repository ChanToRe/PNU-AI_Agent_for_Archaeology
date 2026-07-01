# 03_시각화.R
# 고고학 자료 탐색적 데이터 분석(EDA) 예시 도표 작성

library(ggplot2)
library(grid)

# 한글 폰트 설정 (Windows 폰트 데이터베이스에 등록)
windowsFonts(Malgun = windowsFont("Malgun Gothic"))

# 기본 테마로 theme_bw() 설정 및 한글 폰트 지정 (텍스트 크기 1.8배 설정: 기본 11 -> 19.8)
custom_theme <- theme_bw(base_size = 19.8, base_family = "Malgun") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11 * 1.8),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "grey80", linewidth = 0.3),
    legend.title = element_text(size = 8 * 1.8),
    legend.text = element_text(size = 7 * 1.8),
    legend.key.size = unit(0.4 * 1.8, "cm")
  )
theme_set(custom_theme)

# 병합 및 저장 함수 정의 (DPI 150 설정, title_size 기본 16 * 1.8)
save_combined_plot <- function(plot_list, main_title, file_path, width = 16, height = 4.5, dpi = 150, title_size = 16 * 1.8) {
  png(file_path, width = width * dpi, height = height * dpi, res = dpi)
  grid.newpage()
  
  # 타이틀 영역(0.12)과 플롯 영역(0.88) 분할
  main_layout <- grid.layout(2, 1, heights = unit(c(0.12, 0.88), "null"))
  pushViewport(viewport(layout = main_layout))
  
  # 1. 큰 타이틀 출력 (title_size 반영)
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.text(main_title, gp = gpar(fontsize = title_size, fontface = "bold", fontfamily = "Malgun"))
  popViewport()
  
  # 2. 개별 플롯 출력 영역
  n_plots <- length(plot_list)
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
  
  # 가로로 n_plots 분할
  sub_layout <- grid.layout(1, n_plots)
  pushViewport(viewport(layout = sub_layout))
  
  for (i in 1:n_plots) {
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = i))
    print(plot_list[[i]], newpage = FALSE)
    popViewport()
  }
  
  popViewport(3)
  dev.off()
}

# 데이터 로드 (실습파일 활용)
df <- read.csv("Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 데이터 전처리 (수치형 컬럼 변환)
df$가마크기_길이 <- as.numeric(df$가마크기_길이)
df$가마크기_폭 <- as.numeric(df$가마크기_폭)

# ---------------------------------------------------------------------------
# 1. 막대 도표 (Bar Chart)
# ---------------------------------------------------------------------------
df_bar <- subset(df, 연소부_재임방식 != "" & 연소부_재임방식 != "-" & 연소부_재임방식 != "?")
df_bar$시설구분 <- ifelse(df_bar$연소부_재임방식 == "무시설식", "무시설식", "유시설식")

p_bar_stack <- ggplot(df_bar, aes(x = 권역, fill = 시설구분)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Stack", x = "권역", y = "가마 개수", fill = "시설구분")

p_bar_dodge <- ggplot(df_bar, aes(x = 권역, fill = 시설구분)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Dodge", x = "권역", y = "가마 개수", fill = "시설구분")

p_bar_ident <- ggplot(df_bar, aes(x = 권역, fill = 시설구분)) +
  geom_bar(position = "identity", alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Identity", x = "권역", y = "가마 개수", fill = "시설구분")

p_bar_fill <- ggplot(df_bar, aes(x = 권역, fill = 시설구분)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Fill", x = "권역", y = "비율", fill = "시설구분")

save_combined_plot(
  list(p_bar_stack, p_bar_dodge, p_bar_ident, p_bar_fill),
  "막대 도표",
  "Results/01_막대도표.png",
  width = 18, height = 5.0, dpi = 150
)

# ---------------------------------------------------------------------------
# 2. 원형 도표 (Pie Chart) - 크기 및 텍스트 절반 축소
# ---------------------------------------------------------------------------
df_pie <- subset(df, 연소부_형식 != "" & 연소부_형식 != "-" & 연소부_형식 != "?")
pie_table <- table(df_pie$연소부_형식)
pie_data <- as.data.frame(pie_table)
colnames(pie_data) <- c("연소부_형식", "Count")

# 크기 축소에 맞춰 base_size = 9.9로 재조정
p_pie_stack <- ggplot(pie_data, aes(x = "", y = Count, fill = 연소부_형식)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Stack", x = NULL, y = NULL, fill = "연소부 형식") +
  theme_void(base_family = "Malgun") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11 * 0.9, face = "bold"),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "grey80", linewidth = 0.3),
    legend.title = element_text(size = 8 * 0.9),
    legend.text = element_text(size = 7 * 0.9)
  )

p_pie_fill <- ggplot(pie_data, aes(x = "", y = Count, fill = 연소부_형식)) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Fill", x = NULL, y = NULL, fill = "연소부 형식") +
  theme_void(base_family = "Malgun") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11 * 0.9, face = "bold"),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "grey80", linewidth = 0.3),
    legend.title = element_text(size = 8 * 0.9),
    legend.text = element_text(size = 7 * 0.9)
  )

save_combined_plot(
  list(p_pie_stack, p_pie_fill),
  "원형 도표",
  "Results/02_원형도표.png",
  width = 5.0, height = 2.5, dpi = 150,
  title_size = 16 * 0.9
)

# ---------------------------------------------------------------------------
# 3. 히스토그램 (Histogram)
# ---------------------------------------------------------------------------
df_hist <- subset(df, !is.na(가마크기_길이) & 권역 %in% c("A", "B", "C"))

p_hist_stack <- ggplot(df_hist, aes(x = 가마크기_길이, fill = 권역)) +
  geom_histogram(position = "stack", bins = 12, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Stack", x = "가마 길이 (m)", y = "빈도", fill = "권역")

p_hist_dodge <- ggplot(df_hist, aes(x = 가마크기_길이, fill = 권역)) +
  geom_histogram(position = "dodge", bins = 12, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Dodge", x = "가마 길이 (m)", y = "빈도", fill = "권역")

p_hist_ident <- ggplot(df_hist, aes(x = 가마크기_길이, fill = 권역)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 12, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Identity", x = "가마 길이 (m)", y = "빈도", fill = "권역")

p_hist_fill <- ggplot(df_hist, aes(x = 가마크기_길이, fill = 권역)) +
  geom_histogram(position = "fill", bins = 12, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Fill", x = "가마 길이 (m)", y = "비율", fill = "권역")

save_combined_plot(
  list(p_hist_stack, p_hist_dodge, p_hist_ident, p_hist_fill),
  "히스토그램",
  "Results/03_히스토그램.png",
  width = 18, height = 5.0, dpi = 150
)

# ---------------------------------------------------------------------------
# 4. 선 도표 (Line Chart)
# ---------------------------------------------------------------------------
df_line_prep <- subset(df, !is.na(가마크기_길이) & 연소부_형식 %in% c("①", "②", "③") & 권역 %in% c("A", "B", "C"))
df_line <- aggregate(가마크기_길이 ~ 권역 + 연소부_형식, data = df_line_prep, FUN = mean)

p_line_ident <- ggplot(df_line, aes(x = 연소부_형식, y = 가마크기_길이, color = 권역, group = group <- 권역)) +
  geom_line(position = "identity", linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Identity", x = "연소부 형식", y = "평균 가마 길이 (m)", color = "권역")

p_line_stack <- ggplot(df_line, aes(x = 연소부_형식, y = 가마크기_길이, color = 권역, group = group <- 권역)) +
  geom_line(position = "stack", linewidth = 1.2) +
  geom_point(position = "stack", size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Stack", x = "연소부 형식", y = "누적 평균 길이 (m)", color = "권역")

p_line_fill <- ggplot(df_line, aes(x = 연소부_형식, y = 가마크기_길이, color = 권역, group = group <- 권역)) +
  geom_line(position = "fill", linewidth = 1.2) +
  geom_point(position = "fill", size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Fill", x = "연소부 형식", y = "비율", color = "권역")

save_combined_plot(
  list(p_line_ident, p_line_stack, p_line_fill),
  "선 도표",
  "Results/04_선도표.png",
  width = 14.5, height = 5.0, dpi = 150
)

# ---------------------------------------------------------------------------
# 5. 밀도 도표 (Density Plot)
# ---------------------------------------------------------------------------
df_dens <- subset(df, !is.na(가마크기_길이) & 권역 %in% c("A", "B", "C"))

p_dens_stack <- ggplot(df_dens, aes(x = 가마크기_길이, fill = 권역)) +
  geom_density(position = "stack", alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Stack", x = "가마 길이 (m)", y = "밀도", fill = "권역")

p_dens_ident <- ggplot(df_dens, aes(x = 가마크기_길이, fill = 권역)) +
  geom_density(position = "identity", alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Identity", x = "가마 길이 (m)", y = "밀도", fill = "권역")

p_dens_fill <- ggplot(df_dens, aes(x = 가마크기_길이, fill = 권역)) +
  geom_density(position = "fill", alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Fill", x = "가마 길이 (m)", y = "비율", fill = "권역")

save_combined_plot(
  list(p_dens_stack, p_dens_ident, p_dens_fill),
  "밀도 도표",
  "Results/05_밀도도표.png",
  width = 14.5, height = 5.0, dpi = 150
)

# ---------------------------------------------------------------------------
# 6. 상자 도표 (Box Plot) - 크기 및 텍스트 절반 축소
# ---------------------------------------------------------------------------
df_box <- subset(df, !is.na(가마크기_길이))
df_box$대옹_출토 <- ifelse(df_box$대옹_출토 == "●", "출토", "미출토")

# 크기 축소에 맞춰 base_size = 9.9로 재조정
p_box_dodge2 <- ggplot(df_box, aes(x = 대옹_출토, y = 가마크기_길이, fill = 대옹_출토)) +
  geom_boxplot(position = "dodge2") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Dodge2", x = "대옹 출토여부", y = "가마 길이 (m)", fill = "대옹 출토여부") +
  theme_bw(base_size = 9.9, base_family = "Malgun") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11 * 0.9),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "grey80", linewidth = 0.3),
    legend.title = element_text(size = 8 * 0.9),
    legend.text = element_text(size = 7 * 0.9),
    legend.key.size = unit(0.4 * 0.9, "cm")
  )

save_combined_plot(
  list(p_box_dodge2),
  "상자 도표",
  "Results/06_상자도표.png",
  width = 3.5, height = 2.5, dpi = 150,
  title_size = 16 * 0.9
)

# ---------------------------------------------------------------------------
# 7. 산점도 (Scatter Plot)
# ---------------------------------------------------------------------------
df_scat <- subset(df, !is.na(가마크기_길이) & !is.na(가마크기_폭) & 권역 %in% c("A", "B", "C"))

p_scat_ident <- ggplot(df_scat, aes(x = 가마크기_폭, y = 가마크기_길이, color = 권역)) +
  geom_point(position = "identity", size = 2.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Identity", x = "가마 폭 (m)", y = "가마 길이 (m)", color = "권역")

p_scat_jitter <- ggplot(df_scat, aes(x = 가마크기_폭, y = 가마크기_길이, color = 권역)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), size = 2.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Jitter", x = "가마 폭 (m)", y = "가마 길이 (m)", color = "권역")

p_scat_dodge <- ggplot(df_scat, aes(x = 가마크기_폭, y = 가마크기_길이, color = 권역)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Dodge", x = "가마 폭 (m)", y = "가마 길이 (m)", color = "권역")

save_combined_plot(
  list(p_scat_ident, p_scat_jitter, p_scat_dodge),
  "산점도",
  "Results/07_산점도.png",
  width = 14.5, height = 5.0, dpi = 150
)

cat("모든 예시 도표 생성이 완료되었습니다.\n")
