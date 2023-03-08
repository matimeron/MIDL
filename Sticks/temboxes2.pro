Pro temboxes2, border=bor

	on_error, 1

	fir = !pcol.lblue
	sec = !pcol.blue

	Box, [0,10],[0,8], truasp='xlo', bord = bor
	Rectan, xli=[1,5], yli = [1,4], thi = 4, col=fir
	Rectan, xli = [4.5,7], yli=[1.5,4.3], thi=4, col=fir
	Rectan, xli = [1.,7], yli = [1.5,4], thi=2, col=sec, line=2
	Rectan, xli = [1.,7], yli = [1.5,4], thi=2, col=!pcol.cyan,$ 
	/fill, /line_fill, orien=30
	return
end