Function Pr_struct, type

    nhmx = 512
    nmx = 255

    p = {prodat, type: Default(type,0l,/dtype), $
	cur: 1e-3, dist: 1., conv: 0l, win: fltarr(2), $
	ban: 1e-3, absor: '  ', sin_ang: 1., xtilt: 0l, $
	nfilts: 0l, filts: replicate('  ',8), filths: fltarr(8), $
	nmirrs: 0l, mirrs: replicate('  ',8), mirans: fltarr(8), $
	extra: string(' ', format='(a32)'), nparams: 0l, params: fltarr(8)}

    return, p
end
