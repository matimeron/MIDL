Pro Temopt

	on_error, 1

	Box, [-1,1], [-1,1], truasp = 'ycen'
	plots, [-2,-0.4],[0,0.25], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-2,-0.4],[0,-0.25], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-0.4,0.8],[0.25,0.05], line = 2, thi=1, col = !pcol.lred, noclip=0
	plots, [-0.4,0.8],[-0.25,-0.05], line = 2, thi=1, col = !pcol.lred, noclip=0
	plots, [-0.4,0.5],[0.25,0.1], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-0.4,0.5],[-0.25,-0.1], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [0.5,0.8],[0.03,0.01], line =2, thi=3, col=!pcol.green, noclip=0
	plots, [0.5,0.8],[-0.03,-0.01], line =2, thi=3, col=!pcol.green, noclip=0
	Arc_mm, [-0.4,-0.3],[-0.4,0.3], rad=0.9, shape =sh1, /no
	Arc_mm, [-0.4,0.3],[-0.40,-0.3], rad=0.9, shape=sh2, /no
	lens = [[sh1],[sh2]]
	plots, lens
	polyfill, lens, col = !pcol.blue
	scen = [0.5,0]
	sgen = Shape_close([[0,0],[0.03,0.15],[-0.03,0.15]])
	s1t = Shape_trans(sgen,0,1,scen + [0,0.03])
	s1b = Shape_trans(sgen,!pi,1,scen - [0,0.03])
	polyfill, s1t, color = !pcol.dred
	polyfill, s1b, color = !pcol.dred
	plots, [0.8,0.8], [-0.05,0.05], col = !pcol.red, thi=2
	plots, [0.8,0.8], [-0.01,0.01], col = !pcol.red, thi=5
	Arro, from = [-0.4,-0.35], to = [0.8,-0.35], /two
	Arro, from = [0.5,-0.45], to = [0.8,-0.45], /two
	Labels, [0.4,0.65], [-0.45,-0.55], align=0.5, charsize=2, charthi=2, $
	['L','!7D!xL']

	return
end