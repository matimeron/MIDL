Pro angshow, phi1, phi2, border = bor

	Box, [-1,1], [-0.5,1], tru='ylo', xmar=[3,3], ymar=[2,2], border=bor

	loff = 0.7
	rphi1 = !dtor*phi1
	rphi2 = !dtor*phi2
	Rectan, xli = [-0.05,0.0], yli=[-0.1,0.1], /no_sho, shape = gcr
	p0 = [0,-0.3]
	p1 = [0,0]
	p2 = loff*[sin(2*rphi1),cos(2*rphi1)]
	p3 = p2 + loff/2*[-sin(2*(rphi2-rphi1)),cos(2*(rphi2-rphi1))]
	c1 = [0.6,0.2]
	fcr = Shape_trans(gcr,-rphi1,1)
	scr = Shape_trans(gcr,!pi-2*rphi1+rphi2,1,loff*[sin(2*rphi1),cos(2*rphi1)])	

	polyfill, fcr, col = !pcol.lblue
	plots, fcr, col = !pcol.blue, thi=2, _extra = _e
	polyfill, scr, col = !pcol.lblue
	plots, scr, col = !pcol.blue, thi=2, _extra = _e
	arro, from = p0, to = p1, thi = 2, line = 1, col = !pcol.red
	arro, from = p1, to = p2, thi = 2, line = 1, col = !pcol.red
	arro, from = p2, to = p3, thi = 2, line = 1, col = !pcol.red
	arro, from = c1, to = c1 + 0.2*[1,0], thi=1
	arro, from = c1, to = c1 + 0.2*[0,1], thi=1
	arro, from = p1, to = p1 + 0.4*[cos(rphi1),-sin(rphi1)]
	arro, from = p1, to = p1 +0.4*[1,0]
	arro, from = p2, to = p2 +0.4*[-cos(2*rphi1-rphi2),sin(2*rphi1-rphi2)]
	arro, from = p2, to = p2 +0.4*[1,0]
	

	return
end