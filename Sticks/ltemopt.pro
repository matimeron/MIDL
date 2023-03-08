Pro LTemopt, border = bor

	on_error, 1

	Box, [-1,1], [-1,1], truasp = 'ycen', border=bor
	plots, [-0.8,-0.4],[0.4,0.4], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-0.8,-0.4],[0.2,0.2], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-0.8,-0.4],[0.0,0.0], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-0.8,-0.4],[-0.2,-0.2], line = 2, thi=3, col = !pcol.green, noclip=0
	plots, [-0.8,-0.4],[-0.4,-0.4], line = 2, thi=3, col = !pcol.green, noclip=0

	plots, [-0.4,0.8],[0.4,0.], line =2, thi=3, col=!pcol.green, noclip=0
	plots, [-0.4,0.8],[0.2,0.], line =2, thi=3, col=!pcol.green, noclip=0
	plots, [-0.4,0.8],[0.0,0.], line =2, thi=3, col=!pcol.green, noclip=0
	plots, [-0.4,0.8],[-0.2,0.], line =2, thi=3, col=!pcol.green, noclip=0
	plots, [-0.4,0.8],[-0.4,0.], line =2, thi=3, col=!pcol.green, noclip=0
	Arc_mm, [-0.4,-0.5],[-0.4,0.5], rad=2, shape =sh1, /no
	Arc_mm, [-0.4,0.5],[-0.4,-0.5], rad=2, shape=sh2, /no
	lens = [[sh1],[sh2]]
	plots, lens
	polyfill, lens, col = !pcol.blue
	Arro, from = [-0.4,-0.55], to = [0.8,-0.55], /two
	Arro, from = [-0.9,0], to = [-0.9,0.4]
	Labels, [0.2,-0.95], [-0.65,0.18], align=0.5, charsize=2, charthi=2, $
	['F','x']

	return
end