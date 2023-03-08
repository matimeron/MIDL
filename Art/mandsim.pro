Function Mandsim, const, npoints, niter, zsize, $
    center = zcent, log = lg, noprint = nop

    const = complex(Default(const,0))
    npoints = long(Default(npoints,127) > 2)
    niter = long(Default(niter,50))
    zsize = float(Default(zsize,4.))
    zcent = complex(Default(zcent,0.))
    maxval = abs(const) + .5 + sqrt(abs(const) + .25)
    maxval = maxval > (abs(zcent) + sqrt(.5)*zsize)

    tem = zsize/(npoints-1)*findgen(npoints) - zsize/2
    x = float(zcent) + tem
    y = imaginary(zcent) + tem
    z = complexarr(npoints,npoints)
    for i = 0, npoints - 1 do begin
	z(*,i) = complex(x,y(i))
    endfor
    val = intarr(npoints,npoints)

    idum = call_external('MANDSIM', 'MANDSIM', $
    npoints^2, niter, const, z, val, maxval, $
    default = 'MIDLSHARE:MANDSIM.EXE')

    if not keyword_set(nop) then begin
	if keyword_set(lg) then val = alog(val)
	tvscl, val
    endif

    return, idum
end
