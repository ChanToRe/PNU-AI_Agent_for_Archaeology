# 03_1_막대도표.R
# 고고학 자료 탐색적 데이터 분석(EDA) - 막대 도표 예시 작성

library(ggplot2)
library(grid)

# 한글 폰트 설정 (Windows 폰트 데이터베이스에 등록)
windowsFonts(Malgun = windowsFont("Malgun Gothic"))

# 기본 테마로 theme_bw() 설정 및 한글 폰트 지정 (텍스트 크기 1.8배 설정)
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

# 병합 및 저장 함수 정의 (DPI 150 설정)
save_combined_plot <- function(plot_list, main_title, file_path, width = 16, height = 4.5, dpi = 150, title_size = 16 * 1.8) {
  png(file_path, width = width * dpi, height = height * dpi, res = dpi)
  grid.newpage()
  
  main_layout <- grid.layout(2, 1, heights = unit(c(0.12, 0.88), "null"))
  pushViewport(viewport(layout = main_layout))
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.text(main_title, gp = gpar(fontsize = title_size, fontface = "bold", fontfamily = "Malgun"))
  popViewport()
  
  n_plots <- length(plot_list)
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
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

# 데이터 로드
df <- read.csv("Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 막대도표용 전처리 (무시설식 이외에는 모두 유시설식으로 가공)
df_bar <- subset(df, 연소부_재임방식 != "" & 연소부_재임방식 != "-" & 연소부_재임방식 != "?")
df_bar$시설구분 <- ifelse(df_bar$연소부_재임방식 == "무시설식", "무시설식", "유시설식")

# 4개 Position 그래프 작성
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

# 병합 이미지 저장
save_combined_plot(
  list(p_bar_stack, p_bar_dodge, p_bar_ident, p_bar_fill),
  "막대 도표",
  "Results/01_막대도표.png",
  width = 18, height = 5.0, dpi = 150
)

cat("막대 도표 생성이 완료되었습니다.\n")
