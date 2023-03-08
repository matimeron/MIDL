Pro Right, radius = rad, _extra = _e

	on_error, 1

	x = [ 0.00,  1.00,  2.00,  3.00,  4.00,  5.00, $
		  6.00,  7.00,  8.00,  9.00, 10.00, 11.00, $
		 12.00, 13.00, 14.00, 15.00, 16.00, 17.00, $
		 18.00, 19.00, 20.00, 21.00, 22.00, 23.00, $
		 24.00, 25.00, 26.00]

	tang = [0.855689, 0.780158, 0.722652, 0.848308, 1.93226, 3.03373, $
			 4.10292,  5.43973,  7.31316,  9.42486, 11.4945, 13.4050, $
			 15.1513,  16.7684,  18.2955,  19.7660, 21.2101, 22.6585, $
			 24.1501,  25.7444,  27.5508,  29.8227, 33.3876, 41.4000, $
			 60.7986,  90.0000,  90.0000]

	basang = 2
;	ang = [2,4,4,14,29]
	ang = [2,4,4,10,29]
	rbasang = !dtor*basang
	rang = !dtor*ang
;	len = [145.,35,25,30,20]
	len = [145.,35,25,35,15]
	step = 1.

	a = [0.,0]
	b = a + len[0]*[sin(rang[0]),cos(rang[0])]/cos(rang[0] - rbasang)
	c = b + len[1]*[sin(rang[1]),cos(rang[1])]/cos(rang[1] - rbasang)
	d = a + (len[0]+len[1])*[sin(rang[0]),cos(rang[0])]/cos(rang[0]-rbasang)+ $
		step*[cos(rang[0]),-sin(rang[0])]/cos(rang[0] - rbasang)
	e = d + len[2]*[sin(rang[2]),cos(rang[2])]/cos(rang[2] - rbasang)
	f = e + len[3]*[sin(rang[3]),cos(rang[3])]/cos(rang[3] - rbasang)
	g = f + len[4]*[sin(rang[4]),cos(rang[4])]/cos(rang[4] - rbasang)

	points = [[a],[b],[c],[d],[e],[f],[g]]
	spox = reform(points[0,*])
	pox = spox[[0,1,1,2,2,3,3,4,4,5,5,6]]
	poa = ang[[0,0,1,1,1,1,2,2,3,3,4,4]]
	tpoa = poa
	tpoa[0:4] = tpoa[0:4]/2.

	plot, x, tang, /nodata, _extra = _e
	oplot, x, tang,thi = 2, col = !pcol.red
	plots, pox, poa, thi = 2
	plots, pox, tpoa, line = 2, thi = 2
;	stop
	if Isnum(rad) then begin
		print, spox
		if n_elements(rad) eq 1 then rad = [rad,rad]
		dl = rad[0]*tan((rang[3]-rang[2])/2)
		ofs = $
		[-sin(rang[2])/cos(rang[2]- rbasang),sin(rang[3])/cos(rang[3]- rbasang)]
		rx = spox[4] + [dl*[ofs[0],0,ofs[1]]]
		ry = [ang[2],0.5*(ang[2]+ang[3]),ang[3]]
		plots, rx, ry, line=1, thi = 2
		print, dl
		dl = rad[1]*tan((rang[4]-rang[3])/2)
		ofs = $
		[-sin(rang[3])/cos(rang[3]- rbasang),sin(rang[4])/cos(rang[4]- rbasang)]
		rx = spox[5] + [dl*[ofs[0],0,ofs[1]],len[4]*ofs[1]]
		ry = [ang[3],0.5*(ang[3]+ang[4]),ang[4],$
		0.5*(ang[4]+ang[3]+ len[4]/dl*(ang[4]-ang[3]))]
		plots, rx, ry, line=1, thi = 2
		print, dl
		print
	end

	print, max(pox), total(len) + 20

	return
end