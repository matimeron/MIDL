lam = !srcon.conv/7.36
alp = 0.13
one = id8_pd_read(det_start=[140,25],det_loc=[105,25],pd_cent=[81,29],alp=alp,lam = lam, /sho)
two = id8_pd_read(det_start=[140,25],det_loc=[105,59.4],pd_cent=[81,29],alp=alp,lam = lam, /sho)
three = id8_pd_read(det_start=[140,25],det_loc=[77,25],pd_cent=[81,29],alp=alp,lam = lam, /sho)
four = id8_pd_read(det_start=[140,25],det_loc=[77,59.4],pd_cent=[81,29],alp=alp,lam = lam, /sho)
all = img_join(img_join(one,two,/ver),img_join(three,four,/ver),/hor)