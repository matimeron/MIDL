Pro H2V_crys, ang, coff, radians = rad, border = bor

	on_error, 1

	Box, [-1,1], [-0.5,1], tru='ycen', xmar=[4,4], ymar=[2,2], border=bor

	Rectan, xli=[-0.9,0.9], yli=[-0.4,-0.2], shape= slide, /no_show
	Rectan, xli=[-0.7,0.3], yli=[-0.2,0.1], shape= tilt, /no_show
	Arc_mm, [-0.6,0.12], [0.2,0.12], rad=0.8, shape=arc, /no_show
	arc = Shape_close(arc)
	wedge = Shape_close([[0.1,0.12],[-0.5,0.12],[-0.5,0.37+0.1],[0.1,0.12+0.1]])
	Rectan, xli=[-0.4,0.4], yli=[0,0.1], shape=cryst,/no_show
	cryst = Shape_trans(cryst,-22.75,1,[-0.18,0.346],/cen,/deg)

	Plvar_keep, act='sav'

	polyfill, slide, col= Grey(0.9)
	plots, slide, col=Grey(0.3), thi=2
	polyfill, tilt, col= Grey(0.8)
	plots, tilt, col=Grey(0.3), thi=2
	polyfill, arc, col= Grey(0.7)
	plots, arc, col= Grey(0.3), thi=2
	polyfill, wedge, col= Grey(0.6)
	plots, wedge, col = Grey(0.3), thi=2
	Polyfill, cryst, col=!pcol.lblue
	plots, cryst, col=!pcol.blue, thi=2
	Arro, from = [1,0.45], to = [-0.16,0.45], size=2, thi=3, col=!pcol.cyan
	Arro, from = [-0.16,0.45], to = [-0.16,0.45] + [-0.6,0.6], $
	size=2, thi=3, col=!pcol.cyan

	Labels, [0,-0.2,-0.2,-0.2], [-0.32,-0.1,0.18,0.39], $
		['Linear slide','Tilt stage','Crystal mount','Crys.'], $
		charsize = 1.8, charthi = 1.6, align = 0.5

	Plvar_keep, act='res'
	
	return
end
