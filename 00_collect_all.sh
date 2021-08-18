# collect all greek text from three sources
# 1: Diorisis
# 2: trismegistos
# 3: tlg

cat ../diorisis/out.01.merged/greek_text_only.txt ../trismegistos/out.01.merged/greek_text_only.txt ../tlg/out.02.merged/greek_text_only.txt > data/all.txt
