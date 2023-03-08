Pro DCD_pic_n2, bor=bor, ang = ang

	on_error, 1

	window, 0, xsize=512, ysize = 640

	Plvar_keep, act='sav'
	!x.margin = [4,4]
	Box, [-0.6,0.2], [-0.1,1.1], truasp = 'xlo', border=bor
	dftet = ang
	ftet = dftet*!dtor
	ene = Mono_energy(dftet,crys='ge',ind=[1,1,1])
	stet = Bragg_angle(ene=ene,crys='ge',ind=[2,2,0],/rad)
	psi = 2*(stet-ftet)
	dis = 0.8
	dr = [0.05,0.07]

	c = [0,0]
	cc = c + dr
	f = c + [0,dis]
	g = c + dis*[-sin(psi),cos(psi)]
	tem = [[f-cc],0]
	rtem = Mat_rot(psi,ax=[0,0,1])##tem + cc
	gg = rtem[0:1]
	del = [total(dr*[1-cos(psi),-sin(psi)]),0]
	tem = [[f+del-cc],0]
	rtem = Mat_rot(psi,ax=[0,0,1])##tem + cc
	h = rtem[0:1]

	xx = [c[0],cc[0],f[0],g[0],gg[0],h[0]]
	yy = [c[1],cc[1],f[1],g[1],gg[1],h[1]]

	rectan, xlim=f[0]+[-0.1,0.1], ylim=f[1]+[0,0.01],/fil,col=!pcol.blue
	rectan, xlim=g[0]+[-0.1,0.1], ylim=g[1]+[0,0.01],/fil,col=!pcol.blue, $
	rotate = psi, rotation_center=g
	rectan, xlim=gg[0]+[-0.1,0.1], ylim=gg[1]+[0,0.01],/fil,col=!pcol.lblue, $
	rotate = psi, rotation_center=gg


	plots, c, psym=8, thi=2, symsize=1.5
	plots, cc, psym=8, thi=2, symsize=1.5
	plots, f, psym=8, thi=2, symsize=1.5
	plots, g, psym=8, thi=2, symsize=1.5
	plots, gg, psym=8, thi=2, symsize=1.5
	plots, h, psym=8, thi=2, symsize=1.5

	Arc_mm, c + 0.2*(f-c), cen=c, ang= psi, line=1, thi=2

	Arro, from=c-0.1*(f-c), to=c, thi=2, line=2, siz=0
	Arro, from=c-0.1*(g-c), to=c, thi=2, line=2, siz=0
	Arro, from=c, to=f, thi=2, size=2
	Arro, from=c, to=g, thi=2, size=2

	s = c + [-0.4,0.05]
	Arro, from= s, to= s+ [0.15,0], thi=1, siz=1
	Arro, from= s, to= s + [0,0.15], thi=1, siz=1

	psyms
	
	Labels, xx+[0.04,0.04,0.04,-0.03,0.03,-0.01], $
		yy+[-0.01,-0.01,-0.04,0.02,-0.03,-0.05],$
		align=0.5,charsize=1.6,charthi=1.5, ['C',"C'",'F','G',"G'",'H']
	Labels, c[0]-0.065, c[1]+0.1, charsize= 1.8, charthi=1.5, ['!72h!n!x']
	Labels,[s[0]+0.1,s[0]-0.03],[s[1]-0.03,s[1]+0.1],charsiz=1.4,charthi=1.5,$
		 ['x','y']

	Plvar_keep, act='res'

	return
end