Function Mandgen, const, npoints, niter, zsize, maxval, $
    center = zcent, power = ipow, log = lg, noprint = nop

    ipow = long(Default(ipow,2))
    const = complex(Default(const,replicate(0,ipow)))
    ncon = n_elements(const)
    if ncon lt ipow then const = [const,replicate(complex(0),ipow - ncon)]
    npoints = long(Default(npoints,127) > 2)
    niter = long(Default(niter,32))
    zsize = float(Default(zsize,4.))
    maxval = float(Default(maxval,1e3))
    zcent = complex(Default(zcent,-const(ipow-1)/2.))

    tem = zsize/(npoints-1)*findgen(npoints) - zsize/2
    x = float(zcent) + tem
    y = imaginary(zcent) + tem
    z = complexarr(npoints,npoints)
    for i = 0, npoints - 1 do begin
	z(*,i) = complex(x,y(i))
    endfor
    val = intarr(npoints,npoints)

    idum = call_external('MANDGEN', 'MANDGEN', $
    ipow, npoints^2, niter, [complex(1),reverse(const)], z, val, maxval, $
    default = 'MIDLSHARE:MANDGEN.EXE')

    if not keyword_set(nop) then begin
	if keyword_set(lg) then val = alog(val)
	tvscl, val
    endif

    return, idum
end
