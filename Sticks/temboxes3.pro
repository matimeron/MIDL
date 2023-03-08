Pro temboxes3, border=bor

	on_error, 1

	fir = !pcol.lblue
	sec = !pcol.blue

	Box, [0,10],[0,8], truasp='xlo', bord = bor
	Rectan, xli=[1,5], yli = [1.5,4], thi = 4, col=fir
	Rectan, xli=[1,5], yli = [1.5,4], thi = 2, col=!pcol.red, /fill, $
	/line_fill, spac=.4
	
	Rectan, xli = [4.5,7], yli=[1.5,4.], thi=4, col=fir
	Rectan, xli = [4.5,7], yli=[1.5,4.], thi=1, col=!pcol.green, /fill, $
	/line_fill, spac=0.4
	
	return
end