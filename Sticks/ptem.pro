Pro ptem, border = bor

	on_error, 1

	window, 0
	Box, [0,1], [0,1], tru='xlo', bor=bor
	oshap = [[0.4,0.2],[0.9,0.2],[0.6,0.6],[0.4,0.6]]
	shap = [[0.5,0.2],[0.9,0.2],[0.6,0.6],[0.5,0.6]]
	arro, from=[0.6,0.2], to = [0.6,0.6], /two, siz=1.5
	arro, from=[0.75,0.2], to = [0.75,0.4], /two, siz=1.5
	arro, from=[0.75,0.17], to=[0.6,0.17], /two, siz=1.5
	arc_mm, [0.6,0.48], cen=[0.6,0.6], ang=35, thi = 1, /deg
	plots, oshap, line = 2, thi=2, col=!pcol.blue
	plots, shap, thi=2, col = !pcol.blue
	labels, [0.56,0.645,0.607,0.67],[0.3,0.3,0.51,0.13],['D','d(x)','!7d!x','x'],$
	charsize=2, charthi=2

	window, 1
	Box, [0,1], [0,1], tru='xlo', bor=bor
	arro, from=[0.6,0.2], to = [0.6,0.6], /two, siz=1.5
	arro, from=[0.75,0.27], to = [0.75,0.53], /two, siz=1.5
	arro, from=[0.75,0.17], to=[0.6,0.17], /two, siz=1.5
	arc_mm, [0.6,0.6], cen = [0.6,0.4],ang=360,/deg,thi=2,line=2,col=!pcol.blue
	arc_mm, [0.6,0.6], cen = [0.6,0.4], ang=-180, /deg, thi=2, col=!pcol.blue
	labels, [0.56,0.645,0.67],[0.35,0.35,0.13],['D','d(x)','x'],$
		charsize=2, charthi=2

	wset, 0
	return
end