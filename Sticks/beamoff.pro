Pro Beamoff, dx, dy, border = bor

	on_error, 1

	Box, [-0.8,1.2], [-1,1], truasp = 'ycen', border = bor
	Circle_mm, cen = [0,0], rad=0.6, /no_sh, shap = outcirc
	Circle_mm, cen = [0,0], rad=0.5, /no_sh, shap = incirc
	stage = [[outcirc],[incirc]]
	plots, incirc, thi=4, col=!pcol.dblue
	plots, outcirc, thi=4, col=!pcol.dblue
	polyfill, stage, col=!pcol.lblue
	plots, [0,0], psym=1, symsize=1.5, col=!pcol.dgreen, thi=2
	plots, [0.2,0.15], psym=8, symsize = 1.5, thi=2, col=!pcol.red
	plots, [0.6,0.6], psym=1, symsize=1.5, col=!pcol.dgreen, thi=2
	plots, [0.6,0.5], psym=8, symsize = 1.5, thi=2, col=!pcol.red

	c1 = [0.6,-0.7]
	arro, from = c1, to = c1 + 0.2*[1,0], thi=1
	arro, from = c1, to = c1 + 0.2*[0,1], thi=1
	arro, from = [0,0], to = [0.2,0], line=1, thi=2
	arro, from = [0,0], to = [0.0,0.15], line=1, thi=2

	Labels,[.69,0.55,0.07,-0.1,0.66,0.66],[-0.75, -0.62,-0.06, 0.06,0.58,0.48],$
	['x','z','!7D!xx','!7D!xz','!7v!x-center','beam'], $
	align=0., charsize=1.6,charthi=1.5
	psyms
	return
end