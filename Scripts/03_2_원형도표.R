# 03_2_원형도표.R
# 고고학 자료 탐색적 데이터 분석(EDA) - 원형 도표 예시 작성

library(ggplot2)
library(grid)

# 한글 폰트 설정 (Windows 폰트 데이터베이스에 등록)
windowsFonts(Malgun = windowsFont("Malgun Gothic"))

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

# 원형도표용 전처리
df_pie <- subset(df, 연소부_형식 != "" & 연소부_형식 != "-" & 연소부_형식 != "?")
pie_table <- table(df_pie$연소부_형식)
pie_data <- as.data.frame(pie_table)
colnames(pie_data) <- c("연소부_형식", "Count")

# 크기 축소에 맞게 테마의 base_size = 9.9로 재조정
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

# 병합 이미지 저장 (폭 5.0, 높이 2.5, 타이틀 스케일 0.9)
save_combined_plot(
  list(p_pie_stack, p_pie_fill),
  "원형 도표",
  "Results/02_원형도표.png",
  width = 5.0, height = 2.5, dpi = 150,
  title_size = 16 * 0.9
)

cat("원형 도표 생성이 완료되었습니다.\n")
