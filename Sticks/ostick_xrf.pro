Pro OStick_xrf, alpha = alp, border = bord

	on_error, 1

	glim = [-1,1]
	l1 = 1.
	l3 = 0.8

	rectan, xli = 0.1*glim, yli = 0.02*glim, /no_show, shap = crys
	crys = Shape_trans(crys,0,1,[1,0.02])
	rectan, xli = 0.2*glim, yli = 0.05*glim, /no_show, shap = otrof
	otrof = Shape_trans(otrof,0,1,[0,-0.052])
	rectan, xli = 0.15*glim, yli = 0.03*glim, /no_show, rad= 0.005, shap= itrof
	itrof = Shape_trans(itrof,0,1,[0,-0.03])
	rectan, xli = 0.03*glim, yli = 0.09*glim, /no_show, shap = det
	det = Shape_trans(det,0,1,[-(l3+0.03),0.05])
	sgen = Shape_close([[0,0],[0.02,0.07],[-0.02,0.07]])
	s1t = Shape_trans(sgen,0,1,[0.5,0.01])
	s1b = Shape_trans(sgen,180,1,[0.5,-0.01],/deg)
	s2t = Shape_trans(sgen,0,1,[-0.4,0.01])
	s2b = Shape_trans(sgen,180,1,[-0.4,-0.01],/deg)
	s3t = Shape_trans(sgen,0,1,[-0.6,0.01])
	s3b = Shape_trans(sgen,180,1,[-0.6,-0.01],/deg)

	walp = !dtor*Default(alp,0.,/dtyp)
	v1 = -l1*tan(walp)
	v3 = v1 + l3*sin(walp)
	crys = Shape_trans(crys,walp/2,1,/cen)
	otrof = Shape_trans(otrof,0,1,[0,v1])
	itrof = Shape_trans(itrof,0,1,[0,v1])
	det = Shape_trans(det,-walp,1,[0,v1])
	sgen = Shape_close([[0,0],[0.02,0.07],[-0.02,0.07]])
	s1t = Shape_trans(s1t,walp,1,[0,v1])
	s1b = Shape_trans(s1b,walp,1,[0,v1])
	s2t = Shape_trans(s2t,-walp,1,[0,v1])
	s2b = Shape_trans(s2b,-walp,1,[0,v1])
	s3t = Shape_trans(s3t,-walp,1,[0,v1])
	s3b = Shape_trans(s3b,-walp,1,[0,v1])

	Plvar_keep, act = 'save'
	!x.margin = [5,5]
	!y.margin = [4,4]
	Box, [-1.2,1.2], [-1.2,0.5], truasp = 'ylo', bord= bord

	plots, [[2,0],[l1,0],[0,v1],[-l3*cos(walp),v3]], thi = 4, col = !pcol.cyan
	polyfill, crys, color = !pcol.dgreen
	polyfill, s1t, color = !pcol.purple
	polyfill, s1b, color = !pcol.purple
	polyfill, otrof, color = !pcol.black
	polyfill, itrof, color = !pcol.lblue
	polyfill, s2t, color = !pcol.purple
	polyfill, s2b, color = !pcol.purple
	polyfill, s3t, color = !pcol.purple
	polyfill, s3b, color = !pcol.purple
	Polyfill, det, color = !pcol.dblue
	
	Plvar_keep, act = 'rest'

	return
end
