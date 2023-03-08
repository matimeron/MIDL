Pro Beampath, bor = bor

	on_error, 1

	window, 1, xsize = 512, ysize = 1024
	Box, [-2,2],[-1,15], border=bor, xmar=[4,4], ymar=[2,2], ystyle=5
	psyms
;	plots, [[1.5,0],[0,3],[0,7],[0,14]], psym=8, thi=3, symsize=2
	plots, [1.5,-0.],[0,0], line=2
	plots, [0,0],[0,3],line=2
	plots, [0,0], psym=8, symsize=2
	plots, [1.5,1.5], [0,6], line=2, col=!pcol.red
	plots, [0,-1.],[3,5.], line=2, col=!pcol.red


	Rectan, xlim=[0.,0.05], ylim = [-0.3,0.3], /no_show, shap = crys
	crys1 = Shape_trans(crys,10,1,[1.5,0],cen=[0,0],/deg)
	crys2 = Shape_trans(crys,190,1,[0,3],cen=[0,0],/deg)
	plots, crys1, thi=4, col=!pcol.dblue
	polyfill, crys1, col=!pcol.lblue
	plots, crys2, thi=4, col=!pcol.dblue
	polyfill, crys2, col=!pcol.lblue

	Rectan, xlim=[-0.1,0.1], ylim = [-0.02,0.02], /no_show, shap = bpm
	bpm1 = Shape_trans(bpm,0,1,[0,7],/cen)
	bpm2 = Shape_trans(bpm,0,1,[0,14],/cen)
	plots, bpm1, thi=4, col=!pcol.dgreen
	polyfill, bpm1, col=!pcol.lgreen
	plots, bpm2, thi=4, col=!pcol.dgreen
	polyfill, bpm2, col=!pcol.lgreen

	arro, from = [1.5,-1], to = [1.5,0], col=!pcol.red, thi=3, size=2.5
	arro, from = [1.5,0], to = [0,3], col=!pcol.red, thi=3, size=2.5
	arro, from = [0,3], to = [0,7], col=!pcol.red, thi=3, size=2.5
	arro, from = [0,7], to = [0,14], col=!pcol.red, thi=3, size=2.5
	arro, from = [0,14], to = [0,15], thi=3, col=!pcol.red, size=0
	
	arro, from = [0,5],to = [1.5,5], thi=2, line=2, size= 2, /two
	arro, from = [-1.,0],to = [-1.,7], thi=2, line=2, size= 2, /two
	arro, from = [-1.6,0],to = [-1.6,14], thi=2, line=2, size= 2, /two
	arro, from = [0.8,8], to = [1.6,8], thi=2, size=2
	arro, from = [0.8,8], to = [0.8,9.4], thi=2, size=2

	Labels, $
	[1.65,-0.35,-0.35,-0.35,0.65,-1.35,-1.95,0.7,-0.3,0.9,-0.6,1.1,0.55],$
	[-0.15,2.85,6.85,13.85,0.95,3.4,10.4,5.15,-0.15,1.5,4.5,7.6,8.6],$
	['A','B','C','D','L!d1!n','L!d2!n','L!d3!n','W',"A'",$
	'2!7u!x!d1','2!7u!x!d2','x','y'], charsize=3,charthi=3

	return
end