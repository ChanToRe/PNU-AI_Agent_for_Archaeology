# 03_6_상자도표.R
# 고고학 자료 탐색적 데이터 분석(EDA) - 상자 도표 예시 작성

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

# 데이터 로드 및 수치형 변환
df <- read.csv("Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
df$가마크기_길이 <- as.numeric(df$가마크기_길이)

# 상자도표용 전처리 (권역구분 없이 전체 데이터 사용)
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

# 병합 이미지 저장 (폭 3.5, 높이 2.5, 타이틀 스케일 0.9)
save_combined_plot(
  list(p_box_dodge2),
  "상자 도표",
  "Results/06_상자도표.png",
  width = 3.5, height = 2.5, dpi = 150,
  title_size = 16 * 0.9
)

cat("상자 도표 생성이 완료되었습니다.\n")
