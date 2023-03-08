Pro Lens, border = bor

	on_error, 1

	Box, [-0.8,1.2], [-0.5,0.7], truasp = 'ycen', border = bor

	rad = 0.3
	thi = 0.1
	off = 0.2
	xmax = sqrt((1-thi)*rad)
	x = Make_grid(xmax*[-1,1],256)
	y = x^2/(2*rad) + thi/2
	ymax = max(y)
	pshap = Join_xy(x,y)
	nshap = Shape_trans(pshap,flip='y')
	xright = [xmax, xmax+off,xmax+off,xmax]
	yright = [-ymax,-ymax,ymax,ymax]
	right = Join_xy(xright,yright)
	left = Shape_trans(right,flip='x')
	shap = transpose([transpose(pshap),reverse(transpose(right)), $
		reverse(transpose(nshap)),transpose(left)])

	polyfill, shap, col=!pcol.lblue
	plots, shap, thi=2, col = !pcol.dblue
	Arc_mm, [0,thi/2],cen=[0,thi/2+rad],ang=220, /deg,/sym, $
		thi=3,col=!pcol.dgreen, line=2

	plots, [0,xmax+off+0.1], thi/2 + [0,0], line=1, thi=2
	plots, [0,xmax+off+0.1], -thi/2 + [0,0], line=1, thi=2
	plots, xmax+off+[0,0.2], ymax + [0,0], line=1, thi=2
	plots, xmax+off+[0,0.2], -ymax + [0,0], line=1, thi=2
	plots, xmax+[0,0], ymax + [0,0.2], line=1, thi=2
	plots, -xmax + [0,0], ymax + [0,0.2], line=1, thi=2

	Arro, from = [-xmax,ymax+0.2], to = [xmax,ymax+0.2], thi=1, /two
	Arro, from = [xmax+off+0.1,-thi/2], to = [xmax+off+0.1,thi/2], thi=1, /two
	Arro, from = [xmax+off+0.2,-ymax], to = [xmax+off+0.2,ymax], thi=1, /two
	psi = !pi/6
	Arro, from=[0,thi/2+rad], to=[0,thi/2+rad]+rad*[cos(psi),-sin(psi)],thi=1

	Labels,[0.85,0.95,0,0.15],[-0.02, -0.02,0.72,0.28],$
		['t','d','a','R'], $
		align=0., charsize=1.8,charthi=1.5
	psyms

	return
end