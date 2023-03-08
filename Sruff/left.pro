Pro Left, radius = rad, _extra = _e

	on_error, 1

	x = [ 0.00,  1.00,  2.00,  3.00,  4.00,  5.00, $
		  6.00,  7.00,  8.00,  9.00, 10.00, 11.00, $
		 12.00, 13.00, 14.00, 15.00, 16.00]
	tang = [0.855689, 0.954824, 1.08563, 1.26450, 1.51822, 1.89991, $
			 2.51995,  3.66167, 6.34207, 17.9637, 26.0855, 26.0855, $
			 26.0855,  26.0855, 26.0855, 26.0855, 26.0855]

	basang = 2
	ang = [2,4,4,24]
	rbasang = !dtor*basang
	rang = !dtor*ang
	len = [145.,35,25,25]
	step = 1.

	a = [0.,0]
	b = a + len[0]*[sin(rang[0]),cos(rang[0])]/cos(rang[0] - rbasang)
	c = b + len[1]*[sin(rang[1]),cos(rang[1])]/cos(rang[1] - rbasang)
	d = a + (len[0]+len[1])*[sin(rang[0]),cos(rang[0])]/cos(rang[0]-rbasang)+ $
		step*[cos(rang[0]),-sin(rang[0])]/cos(rang[0] - rbasang)
	e = d + len[2]*[sin(rang[2]),cos(rang[2])]/cos(rang[2] - rbasang)
	f = e + len[3]*[sin(rang[3]),cos(rang[3])]/cos(rang[3] - rbasang)

	points = [[a],[b],[c],[d],[e],[f]]
	spox = reform(points[0,*])
	pox = spox[[0,1,1,2,2,3,3,4,4,5]]
	poa = ang[[0,0,1,1,1,1,2,2,3,3]]
	tpoa = poa
	tpoa[0:4] = tpoa[0:4]/2.

	plot, x, tang, /nodata, _extra = _e
	oplot, x, tang,thi = 2, col = !pcol.red
	plots, pox, poa, thi = 2
	plots, pox, tpoa, line = 2, thi = 2
	if Isnum(rad) then begin
		dl = rad*tan((rang[3]-rang[2])/2)
		ofs = $
		[-sin(rang[2])/cos(rang[2]- rbasang),sin(rang[3])/cos(rang[3]- rbasang)]
		rx = spox[4] + [dl*[ofs[0],0,ofs[1]],len[3]*ofs[1]]
		ry = [ang[2],0.5*(ang[2]+ang[3]),ang[3],$
		0.5*(ang[3]+ang[2]+ len[3]/dl*(ang[3]-ang[2]))]
		plots, rx, ry, line=1, thi = 2
		print, spox
		print, dl
		print
	end

	print, max(pox), total(len) + 20

	return
end