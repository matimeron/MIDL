Pro Stick_mirror_scan, y = y, theta = tet, noise = nos, bord = bord

	on_error, 1

	wtet = Default(tet,0)
	wy = Default(y,0)

	scen = [0.05,0]
	sgen = Shape_close([[0,0],[0.02,0.11],[-0.02,0.11]])
	s1t = Shape_trans(sgen,0,1,scen + [0,0.002])
	s1b = Shape_trans(sgen,!pi,1,scen - [0,0.002])
	rectan, xli=[-0.11,0.11],yli=[-0.02,0.0],/no_show,shap=mir
	rectan, xli=[0.96,1],yli=[0,0.2],/no_show,shap=det
	rectan, xli=[-0.1,scen[0]],yli=[-0.05,0.05],/no_show,shap=ibeam
	mirl = [[-0.11,0],[0.11,0]]
	fmir = Shape_trans(mir,wtet,1,[0.3,0] + [0,wy])
	fmirl = Shape_trans(mirl,wtet,1,[0.3,0] + [0,wy])
	smir = Shape_trans(mir,!pi+wtet,1,[0.5,0.2*tan(2*wtet)] + [0,wy])
	smirl = Shape_trans(mirl,!pi+wtet,1,[0.5,0.2*tan(2*wtet)] + [0,wy])
	beam = [[scen],[scen + [1,0]]]
	dum = Lincross(beam,fmirl,line=0,cross=fir)
	dum = Lincross([[fir],[cos(2*wtet),sin(2*wtet)]],smirl,line=1,cross=sec)
	dtet = Default(nos,0)*randomn(s)
	dum = Lincross([[sec],[cos(2*dtet),sin(2*dtet)]],[[0.96,0],[0.96,0.2]],$
		line=1,cross=thi)

	plvar_keep, act = 'sav'

	box, [0,1], [-0.5,0.5], truasp='ycen', bord = bord, xmar=[2,2], ymar=[2,2]
	polyfill, ibeam, col=!pcol.cyan
	polyfill, s1t, color = !pcol.purple
	polyfill, s1b, color = !pcol.purple
	polyfill, fmir, color = !pcol.dblue
	polyfill, smir, color = !pcol.dblue
	Polyfill, det, color = !pcol.dred
	
	plots, fmirl, thi=2, col=!pcol.lblue
	plots, smirl, thi=2, col=!pcol.lblue
	plots, [[scen],[fir]],thi=4,col=!pcol.cyan
	plots, [[fir],[sec]],thi=4,col=!pcol.cyan
	plots, [[sec],[thi]],thi=4,col=!pcol.cyan

	plvar_keep, act = 'res'

	return
end