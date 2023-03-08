Pro Ftemopt, border=bor

	on_error, 1

	Box, [-1,1], [-1,1], truasp = 'ycen', border=bor
	plots, [-1,0.2],[0.04,0.25], line = 0, thi=3, col = !pcol.green, noclip=0
	plots, [-1,0.2],[-0.04,-0.25], line = 0, thi=3, col = !pcol.green, noclip=0

	plots, [0.2,0.8],[0.25,0.02], line = 0, thi=3, col = !pcol.green, noclip=0
	plots, [0.2,0.8],[-0.25,-0.02], line = 0, thi=3, col = !pcol.green, noclip=0
	plots, [0.2,1],[0.25,0.025], line = 2, thi=3, col = !pcol.dgreen, noclip=0
	plots, [0.2,1],[-0.25,-0.025], line = 2, thi=3, col = !pcol.dgreen, noclip=0
	
	Arc_mm, [0.2,-0.3],[0.2,0.3], rad=0.9, shape =sh1, /no
	Arc_mm, [0.2,0.3],[0.2,-0.3], rad=0.9, shape=sh2, /no
	lens = [[sh1],[sh2]]
	plots, lens
	polyfill, lens, col = !pcol.blue

	plots, [-1,-1], [-0.04,0.04], col = !pcol.red, thi=3
	plots, [0.8,0.8], [-0.02,0.02], col = !pcol.red, thi=3
	plots, [1,1], [-0.025,0.025], col = !pcol.dred, thi=3
	Arro, from = [-1,-0.35], to = [0.2,-0.35], /two, size=1.5, thi=1
	Arro, from = [0.2,-0.35], to = [0.8,-0.35], /two, size=1.5, thi=1
	Arro, from = [0.2,-0.5], to = [1,-0.5], /two, line=2, size=1.5, thi=1
	Labels, [-0.4,0.5,0.6], [-0.45,-0.45,-0.6],align=0.5, charsize=2,charthi=2,$
	['L!d0!n','L!d1!n',"L'!d1!n"]

	return
end