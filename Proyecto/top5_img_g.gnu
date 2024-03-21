set term png
set output "top5_img_g.png"
set title "Top 5 Clientes con Más Imágenes Grandes"
set xlabel "ID del Cliente"
set ylabel "Número de Imágenes Grandes"
set style fill solid
plot "top5_img_g.dat" using 2:xtic(1) with boxes title "img_g" lc rgb "green"
