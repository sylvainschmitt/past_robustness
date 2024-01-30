
#----------------------------------------------#
# Fig.3: model performance across the Holocene #
#----------------------------------------------#

upper_panel <- plot_grid(ordbetareg_plot + theme(legend.position = "none"), 
                         ggdraw() +
                           draw_image(magick::image_read_pdf(file.path(wd, "manuscript", "figs", "fig3", "test.pdf"), density = 600),
                                      x = -0.1, scale = 1.4, clip = "off"), 
                         ncol = 2, rel_widths = c(0.55,0.4), align = "hv", axis = "tblr",
                         labels = c("a", NA), label_fontfamily = "Helvetica Narrow", label_size = 11)

lower_panel <- plot_grid(boxplot_transferability, boxplot_performance, boxplot_sdperformance,
                         ncol = 3, rel_widths = c(0.49, 0.53, 0.33), align = "hv", axis = "tblr",
                         labels = c("b", "c", "d"), label_fontfamily = "Helvetica Narrow", label_size = 11)


fig3_main <- plot_grid(upper_panel, lower_panel, ncol = 1, rel_heights = c(1.3, 1))



