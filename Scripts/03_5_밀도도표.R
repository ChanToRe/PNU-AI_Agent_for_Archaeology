# 03_5_밀도도표.R
# 고고학 자료 탐색적 데이터 분석(EDA) - 밀도 도표 예시 작성

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

# 데이터 로드 및 수치형 변환
df <- read.csv("Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
df$가마크기_길이 <- as.numeric(df$가마크기_길이)

# 밀도도표용 전처리
df_dens <- subset(df, !is.na(가마크기_길이) & 권역 %in% c("A", "B", "C"))

# 3개 Position 그래프 작성 (Dodge는 제외됨)
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

# 병합 이미지 저장
save_combined_plot(
  list(p_dens_stack, p_dens_ident, p_dens_fill),
  "밀도 도표",
  "Results/05_밀도도표.png",
  width = 14.5, height = 5.0, dpi = 150
)

cat("밀도 도표 생성이 완료되었습니다.\n")
