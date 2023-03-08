Pro Crysoff, dx, dy, border = bor

	on_error, 1

	Box, [-0.6,1.4], [-1,1], truasp = 'ycen', border = bor
	Rectan, xlim=[0.,0.1], ylim = [-0.5,0.5], /no_show, shap = crys
	crys = Shape_trans(crys,10,1,[0.2,0.15],cen=[0,0],/deg)
	plots, crys, thi=4, col=!pcol.dblue
	polyfill, crys, col=!pcol.lblue
	plots, [0,0], psym=7, symsize=1.5, col=!pcol.dgreen, thi=2
	plots, [0.2,0.15], psym=7, symsize = 1.5, thi=2, col=!pcol.cyan
	

	plots, [0.6,0.6], psym=7, symsize=1.5, col=!pcol.dgreen, thi=2
	plots, [0.6,0.5], psym=7, symsize = 1.5, thi=2, col=!pcol.cyan

	c1 = [0.7,-0.3]
	arro, from = c1, to = c1 + 0.2*[1,0], thi=1
	arro, from = c1, to = c1 + 0.2*[0,1], thi=1
	arro, from = [0,0], to = [0.2,0], line=1, thi=2
	arro, from = [0,0], to = [0.0,0.15], line=1, thi=2

	Labels,[.79,0.65,0.07,-0.1,0.66,0.66],[-0.35, -0.22,-0.06, 0.06,0.58,0.48],$
	['x','y','!7D!xx','!7D!xy','correct loc.','offset loc.'], $
	align=0., charsize=1.6,charthi=1.5
	psyms
	return
end