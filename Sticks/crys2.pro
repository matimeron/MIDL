Pro Crys2, ang, coff, radians = rad, border = bor

	on_error, 1

	if keyword_set(rad) then mult = 1 else mult = !dtor
	wang = mult*Default(ang,0)
	coff = Default(coff,0.)
	cofl = coff ne 0

	Box, [-1,1], [-1,1], tru='ycen', xmar=[3,3], ymar=[2,2], border=bor

	Rectan, xli = [0,0.7], yli=[-1,1], /no_sho, shape = scr
	rcr = Shape_trans(scr,wang-!pi,1,cen=[0,0])

	Plvar_keep, act='sav'

;	plots, scr, noclip=0, line = 2
	polyfill, rcr, col=!pcol.lblue, noclip=0
	plots, rcr, col = !pcol.dblue, thi = 2, noclip=0

	loc = [0,0]
	ori = 1.5*[sin(2*wang),-cos(2*wang)]
	Arro, from =ori,to=loc,size=2,thi=2,col=!pcol.dred,noclip=0
	Arro, from=loc, to=loc + [0,0.5], size=2,thi=2,col=!pcol.dred,noclip=0

	offval = coff*tan(wang/2)
	off1 = offval*[0,1]
	off2 = offval*2*cos(wang)*[-sin(wang),cos(wang)]
	
	Arro, from=ori+off1, to=loc+off2,siz=2,lin=2,thi=2,col=!pcol.dred,noclip=0
	Arro, from=ori-off1, to=loc-off2,siz=2,lin=2,thi=2,col=!pcol.dred,noclip=0
	Arro, from=loc+off2, to=loc+off2 + [0,0.5],size=2,lin=2,thi=2, $
		col=!pcol.dred,noclip=0
	Arro, from=loc-off2, to=loc-off2 + [0,0.5],size=2,lin=2,thi=2, $
		col=!pcol.dred,noclip=0

	Plvar_keep, act='res'

	return
end
