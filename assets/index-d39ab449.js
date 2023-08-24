(function(){const e=document.createElement("link").relList;if(e&&e.supports&&e.supports("modulepreload"))return;for(const a of document.querySelectorAll('link[rel="modulepreload"]'))t(a);new MutationObserver(a=>{for(const u of a)if(u.type==="childList")for(const o of u.addedNodes)o.tagName==="LINK"&&o.rel==="modulepreload"&&t(o)}).observe(document,{childList:!0,subtree:!0});function r(a){const u={};return a.integrity&&(u.integrity=a.integrity),a.referrerPolicy&&(u.referrerPolicy=a.referrerPolicy),a.crossOrigin==="use-credentials"?u.credentials="include":a.crossOrigin==="anonymous"?u.credentials="omit":u.credentials="same-origin",u}function t(a){if(a.ep)return;a.ep=!0;const u=r(a);fetch(a.href,u)}})();const Gh=function(n){return function(e){for(var r=n.length,t=e.length,a=new Array(r*t),u=0,o=0;o<r;o++)for(var i=n[o],c=0;c<t;c++)a[u++]=i(e[c]);return a}};var gi={compose:function(n){return function(e){return function(r){return n(e(r))}}}},wi=function(n){return n.compose},ip=function(n){var e=wi(n);return function(r){return function(t){return e(t)(r)}}},An=function(n){return n.identity},Tn={identity:function(n){return n},Semigroupoid0:function(){return gi}},Kn=function(n){return function(e){return function(r){return n(r)(e)}}},Fn=function(n){return function(e){return n}},oo=function(n){return function(e){return e(n)}},cp=function(n){return function(e){return n(e)}};const Qh=function(n){return function(e){for(var r=e.length,t=new Array(r),a=0;a<r;a++)t[a]=n(e[a]);return t}},w=void 0;var x=function(){function n(){}return n.value=new n,n}(),k=function(n){return n.map},Yr=function(n){var e=k(n);return function(r){return function(t){return e(t)(r)}}},Pe=function(n){return k(n)(Fn(w))},Ae=function(n){var e=k(n);return function(r){return function(t){return e(Fn(t))(r)}}},Yh=function(n){var e=k(n);return function(r){return e(Fn(r))}},io={map:wi(gi)},Ce={map:Qh},Kh=An(Tn),fp={apply:function(n){return function(e){return function(r){return n(r)(e(r))}}},Functor0:function(){return io}},lp={apply:Gh,Functor0:function(){return Ce}},ne=function(n){return n.apply},Jn=function(n){var e=ne(n),r=k(n.Functor0());return function(t){return function(a){return e(r(Fn(Kh))(t))(a)}}},sp=function(n){var e=ne(n),r=k(n.Functor0());return function(t){return function(a){return function(u){return e(r(t)(a))(u)}}}},S=function(n){return n.pure},Xh=function(n){var e=S(n);return function(r){return function(t){if(!r)return t;if(r)return e(w);throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): "+[r.constructor.name,t.constructor.name])}}},Ya=function(n){var e=S(n);return function(r){return function(t){if(r)return t;if(!r)return e(w);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,t.constructor.name])}}},Jh=function(n){var e=ne(n.Apply0()),r=S(n);return function(t){return function(a){return e(r(t))(a)}}},Zh={pure:function(n){return function(e){return n}},Apply0:function(){return fp}},cf={pure:function(n){return[n]},Apply0:function(){return lp}};const nx=function(n){return function(e){for(var r=[],t=0,a=n.length;t<a;t++)Array.prototype.push.apply(r,e(n[t]));return r}};var ex=An(Tn),ff=function(n){return n.discard},co={bind:nx,Apply0:function(){return lp}},Zn=function(n){return n.bind},bi=function(n){return Kn(Zn(n))},lf=function(n){var e=Zn(n);return function(r){return function(t){return function(a){return e(r(a))(t)}}}},sf={discard:function(n){return Zn(n)}},Dt=function(n){var e=Zn(n);return function(r){return e(r)(ex)}};const rx=function(n,e){for(var r=n>e?-1:1,t=new Array(r*(e-n)+1),a=n,u=0;a!==e;)t[u++]=a,a+=r;return t[u]=a,t},vf=function(n){return n.length},tx=function(n,e,r,t){return t<0||t>=r.length?e:n(r[t])},ax=function(n,e,r,t){for(var a=0,u=t.length;a<u;a++)if(r(t[a]))return n(a);return e},ux=function(n,e,r,t){if(r<0||r>=t.length)return e;var a=t.slice();return a.splice(r,1),n(a)},ox=function(n,e,r){for(var t=e.length<r.length?e.length:r.length,a=new Array(t),u=0;u<t;u++)a[u]=n(e[u])(r[u]);return a},ix=function(n,e){return n[e]},cx=function(n){return function(e){return n+e}},fx=function(n){return function(e){return n.length===0?e:e.length===0?n:n.concat(e)}};var da=function(n){return n.reflectSymbol},pf=function(n){var e=function(r){var t;function a(u){}for(;;)t=void 0;return t};return e()};const lx=function(n){return function(e){return{}.hasOwnProperty.call(e,n)}},df=function(n){return function(e){return e[n]}},mf=function(n){return function(e){return function(r){var t={};for(var a in r)({}).hasOwnProperty.call(r,a)&&(t[a]=r[a]);return t[n]=e,t}}};var sx={append:function(n){return function(e){return w}}},vx={append:cx},px={appendRecord:function(n){return function(e){return function(r){return{}}}}},ma={append:fx},vp=function(n){return n.appendRecord},dx=function(){return function(n){return{append:vp(n)(x.value)}}},se=function(n){return n.append},pp=function(n){var e=se(n);return{append:function(r){return function(t){return function(a){return e(r(a))(t(a))}}}}},mx=function(n){var e=da(n);return function(){return function(r){var t=vp(r);return function(a){var u=se(a);return{appendRecord:function(o){return function(i){return function(c){var l=t(x.value)(i)(c),s=e(x.value),p=mf(s),m=df(s);return p(u(m(i))(m(c)))(l)}}}}}}}},ve=function(n){return n.alt},hx=function(n,e,r){var t=0,a;return function(u){if(t===2)return a;if(t===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return t=1,a=r(),t=2,a}},xx={defer:function(n){return function(e){return n(w)(e)}}},yx=function(n){return n.defer},gx=function(n){var e=yx(n);return function(r){var t=hx("go","Control.Lazy",function(){return e(function(u){return r(t(25))})}),a=t(25);return a}},wx=function(n){var e=Zn(n.Bind1()),r=Xh(n.Applicative0());return function(t){return function(a){return e(t)(function(u){return r(u)(a)})}}},fo=function(n){var e=Zn(n.Bind1()),r=S(n.Applicative0());return function(t){return function(a){return e(t)(function(u){return e(a)(function(o){return r(u(o))})})}}};const bx=2147483647,$x=-2147483648;var dp=function(n){return function(e){return function(r){return function(t){return function(a){return t<a?n:t===a?e:r}}}}};const _x=dp,Ox=dp;var mp=function(n){return function(e){return n===e}};const kx=mp,Sx=mp;var Tx={eq:Sx},Px={eq:kx},Ax=function(n){return n.eq1},hf=function(n){return n.eq},er=function(){function n(){}return n.value=new n,n}(),at=function(){function n(){}return n.value=new n,n}(),ze=function(){function n(){}return n.value=new n,n}();const Ex=function(n){return function(e){return n-e|0}},Ix=function(n){return function(e){return n-e}},qx=function(n){return function(e){return n+e|0}},Fx=function(n){return function(e){return n*e|0}},Cx=function(n){return function(e){return n+e}},Bx=function(n){return function(e){return n*e}};var hp=function(n){return n.zero},Rt={add:Cx,zero:0,mul:Bx,one:1},lo={add:qx,zero:0,mul:Fx,one:1},Dx=function(n){return n.one},xp=function(n){return n.mul},yr=function(n){return n.add},yp=function(n){return n.sub},xf={sub:Ix,Semiring0:function(){return Rt}},Rx={sub:Ex,Semiring0:function(){return lo}},Nx=function(n){var e=yp(n),r=hp(n.Semiring0());return function(t){return e(r)(t)}},yf=function(){return{compare:Ox(er.value)(ze.value)(at.value),Eq0:function(){return Tx}}}(),Ka=function(){return{compare:_x(er.value)(ze.value)(at.value),Eq0:function(){return Px}}}(),zx=function(n){return n.compare1},ut=function(n){return n.compare},Lx=function(n){var e=ut(n);return function(r){return function(t){var a=e(r)(t);return a instanceof at}}},Mx=function(n){var e=ut(n);return function(r){return function(t){var a=e(r)(t);return!(a instanceof er)}}},Wx=function(n){var e=ut(n);return function(r){return function(t){var a=e(r)(t);return a instanceof er}}},Ux=function(n){var e=ut(n);return function(r){return function(t){var a=e(r)(t);return!(a instanceof at)}}},gp=function(n){var e=ut(n);return function(r){return function(t){var a=e(r)(t);if(a instanceof er)return t;if(a instanceof ze||a instanceof at)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[a.constructor.name])}}},Hx=function(n){var e=Mx(n);return function(r){var t=hp(r.Semiring0()),a=Nx(r);return function(u){var o=e(u)(t);return o?u:a(u)}}},jx=function(n){return n.top},wp={top:bx,bottom:$x,Ord0:function(){return Ka}},Vx=function(n){return n.bottom};const Gx=function(n){return n.toString()};var ha={show:Gx},xa=function(n){return n.show},Qx=An(Tn),_=function(){function n(){}return n.value=new n,n}(),$=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),pn=function(n){return function(e){return function(r){if(r instanceof _)return n;if(r instanceof $)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[n.constructor.name,e.constructor.name,r.constructor.name])}}},ya={map:function(n){return function(e){return e instanceof $?new $(n(e.value0)):_.value}}},Yx=k(ya),bp=function(n){return pn(n)(Qx)},$p=function(){return function(n){if(n instanceof $)return n.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[n.constructor.name])}},_p={apply:function(n){return function(e){if(n instanceof $)return Yx(n.value0)(e);if(n instanceof _)return _.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[n.constructor.name,e.constructor.name])}},Functor0:function(){return ya}},gf={bind:function(n){return function(e){if(n instanceof $)return e(n.value0);if(n instanceof _)return _.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[n.constructor.name,e.constructor.name])}},Apply0:function(){return _p}},Op=function(){return{pure:$.create,Apply0:function(){return _p}}}(),Kx={alt:function(n){return function(e){return n instanceof _?e:n}},Functor0:function(){return ya}},Xx=function(){return{empty:_.value,Alt0:function(){return Kx}}}(),Qn=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Ln=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Jx={map:function(n){return function(e){if(e instanceof Qn)return new Qn(e.value0);if(e instanceof Ln)return new Ln(n(e.value0));throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): "+[e.constructor.name])}}},Ha=function(n){return function(e){return function(r){if(r instanceof Qn)return n(r.value0);if(r instanceof Ln)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[n.constructor.name,e.constructor.name,r.constructor.name])}}},wf=function(){return Ha(Fn(_.value))($.create)}(),Zx=function(n){return n},bf={map:function(n){return function(e){return n(e)}}},kp={apply:function(n){return function(e){return n(e)}},Functor0:function(){return bf}},ny={bind:function(n){return function(e){return e(n)}},Apply0:function(){return kp}},ey={pure:Zx,Apply0:function(){return kp}},ga={Applicative0:function(){return ey},Bind1:function(){return ny}};const ry=function(n){return Math.min(Math.abs(n),2147483647)},ty=function(n){return function(e){return e===0?0:e>0?Math.floor(n/e):-Math.floor(n/-e)}},ay=function(n){return function(e){if(e===0)return 0;var r=Math.abs(e);return(n%r+r)%r}},uy=function(n){return function(e){return n/e}};var oy={Ring0:function(){return xf}},iy={Ring0:function(){return Rx}},$f=function(n){return n.mod},Sp={degree:function(n){return 1},div:uy,mod:function(n){return function(e){return 0}},CommutativeRing0:function(){return oy}},_f={degree:ry,div:ty,mod:ay,CommutativeRing0:function(){return iy}},Tp=function(n){return n.div},cy=dx(),Pp={mempty:w,Semigroup0:function(){return sx}},Ap={memptyRecord:function(n){return{}},SemigroupRecord0:function(){return px}},Ep=function(n){return n.memptyRecord},Ip=function(){return function(n){var e=cy(n.SemigroupRecord0());return{mempty:Ep(n)(x.value),Semigroup0:function(){return e}}}},Xe=function(n){return n.mempty},fy=function(n){var e=Xe(n),r=pp(n.Semigroup0());return{mempty:function(t){return e},Semigroup0:function(){return r}}},fi=function(n){var e=da(n),r=mx(n)();return function(t){var a=Xe(t),u=t.Semigroup0();return function(){return function(o){var i=Ep(o),c=r(o.SemigroupRecord0())(u);return{memptyRecord:function(l){var s=i(x.value),p=e(x.value),m=mf(p);return m(a)(s)},SemigroupRecord0:function(){return c}}}}}};const qp=function(n){return function(){return n}},ly=function(n){return function(e){return function(){return e(n())()}}},$i=function(n){return function(e){return function(){for(var r=0,t=n.length;r<t;r++)e(n[r])()}}};var Fp=function(n,e,r){var t=0,a;return function(u){if(t===2)return a;if(t===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return t=1,a=r(),t=2,a}},so={Applicative0:function(){return gn},Bind1:function(){return Nt}},Nt={bind:ly,Apply0:function(){return Of(0)}},gn={pure:qp,Apply0:function(){return Of(0)}},Cp=Fp("functorEffect","Effect",function(){return{map:Jh(gn)}}),Of=Fp("applyEffect","Effect",function(){return{apply:fo(so),Functor0:function(){return Cp(0)}}}),Ee=Cp(20),ue=Of(23),sy=sp(ue),Bp=function(n){return{append:sy(se(n))}},vy=function(n){var e=Bp(n.Semigroup0());return{mempty:qp(Xe(n)),Semigroup0:function(){return e}}};const py=function(n){return function(){return{value:n}}},va=function(n){return function(){return n.value}},dy=function(n){return function(e){return function(){var r=n(e.value);return e.value=r.state,r.value}}},ja=function(n){return function(e){return function(){e.value=n}}};var my=Pe(Ee),Qe=py,hy=dy,Uc=function(n){return hy(function(e){var r=n(e);return{state:r,value:r}})},cc=function(n){return function(e){return my(Uc(n)(e))}};const xy=function(n){return function(e){return function(){return n(e())}}},yy=function(n){return function(){return n}},gy=function(n){return function(e){return function(){return e(n())()}}};function J(n){return function(){return{value:n}}}const V=function(n){return function(){return n.value}},wy=function(n){return function(e){return function(){var r=n(e.value);return e.value=r.state,r.value}}},Yn=function(n){return function(e){return function(){return e.value=n}}};var by=function(n,e,r){var t=0,a;return function(u){if(t===2)return a;if(t===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return t=1,a=r(),t=2,a}},$y=wy,ie=function(n){return $y(function(e){var r=n(e);return{state:r,value:r}})},rr={map:xy},Dp={Applicative0:function(){return Le},Bind1:function(){return vo}},vo={bind:gy,Apply0:function(){return kf(0)}},Le={pure:yy,Apply0:function(){return kf(0)}},kf=by("applyST","Control.Monad.ST.Internal",function(){return{apply:fo(Dp),Functor0:function(){return rr}}}),_i=kf(47),_y=sp(_i),Oy=S(Le),ky=function(n){return{append:_y(se(n))}},Sy=function(n){var e=ky(n.Semigroup0());return{mempty:Oy(Xe(n)),Semigroup0:function(){return e}}};function ju(){return[]}const Ty=function(n,e){return e.push.apply(e,n)};function Py(n){return n}const Ay=Py;function Rp(n){return n.slice()}const Ey=Rp,Iy=Rp,Sf=function(e){return function(r){return function(){return e(r)}}},qy=function(e){return function(r){return function(t){return function(){return e(r,t)}}}};var ni=Sf(Ay),Fy=Sf(Iy),Cy=function(n){return function(e){return function(){var t=Fy(e)();return n(t)(),ni(t)()}}},Ma=function(n){return qy(Ty)([n])},Np=Sf(Ey);const By=function(n){return function(e){return n&&e}},Dy=function(n){return function(e){return n||e}},Ry=function(n){return!n};var zp=function(n){return n.not},Ny=function(n){return n.disj},Hc={ff:!1,tt:!0,implies:function(n){return function(e){return Ny(Hc)(zp(Hc)(n))(e)}},conj:By,disj:Dy,not:Ry},Lp=k(rr),zy=zp(Hc),Ly=Pe(rr),My=function(){function n(e,r){this.value0=e,this.value1=r}return n.create=function(e){return function(r){return new n(e,r)}},n}(),Wy=function(n){return function(){var r=V(n.value1)();return ie(function(t){return t+1|0})(n.value1)(),n.value0(r)}},jl=function(n){return Lp(My.create(n))(J(0))},Vl=function(n){return function(e){return function(){for(var t=J(!1)();Lp(zy)(V(t))();)(function(){var u=Wy(n)();if(u instanceof $)return e(u.value0)();if(u instanceof _)return Ly(Yn(!0)(t))();throw new Error("Failed pattern match at Data.Array.ST.Iterator (line 42, column 5 - line 44, column 47): "+[u.constructor.name])})();return{}}}};const Uy=function(n){return function(e){return function(r){for(var t=e,a=r.length,u=a-1;u>=0;u--)t=n(r[u])(t);return t}}},Hy=function(n){return function(e){return function(r){for(var t=e,a=r.length,u=0;u<a;u++)t=n(t)(r[u]);return t}}};var Ir=function(n){return n.empty},q=function(){function n(e,r){this.value0=e,this.value1=r}return n.create=function(e){return function(r){return new n(e,r)}},n}(),jy=function(n){return function(e){return n(e.value0)(e.value1)}},Xa=function(n){return n.value1},Mp={map:function(n){return function(e){return new q(e.value0,n(e.value1))}}},Yu=function(n){return n.value0},Vy=function(n){var e=wi(n);return{append:function(r){return function(t){return e(r)(t)}}}},Wp=function(n){var e=Vy(n.Semigroupoid0());return{mempty:An(n),Semigroup0:function(){return e}}};const qr=function(n){return n};var wa=function(){return qr},Tf=wa(),Pf=function(){return Tf},ba=function(){return Tf},Gy=function(){return function(){return function(n){return Tf}}},Pr=function(n){return n.foldr},ot=function(n){var e=Pr(n);return function(r){return e(ve(r.Alt0()))(Ir(r))}},it=function(n){var e=Jn(n.Apply0()),r=S(n);return function(t){var a=Pr(t);return function(u){return a(function(o){return e(u(o))})(r(w))}}},vr=function(n){var e=it(n);return function(r){return Kn(e(r))}},Pt=function(n){return n.foldl},tr={foldr:function(n){return function(e){return function(r){if(r instanceof _)return e;if(r instanceof $)return n(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[n.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(n){return function(e){return function(r){if(r instanceof _)return e;if(r instanceof $)return n(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[n.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(n){var e=Xe(n);return function(r){return function(t){if(t instanceof _)return e;if(t instanceof $)return r(t.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,t.constructor.name])}}}},Qy=function(n){var e=Pr(n);return function(r){var t=se(r.Semigroup0()),a=Xe(r);return function(u){return e(function(o){return function(i){return t(u(o))(i)}})(a)}}},he={foldr:Uy,foldl:Hy,foldMap:function(n){return Qy(he)(n)}},yu=function(n){return n.foldMap};const Up=function(n){return function(e){return function(r){return n(e,r)}}},Yy=function(n){return function(e){return function(r){return function(t){return n(e,r,t)}}}},Af=function(n){return function(e){return function(r){return function(t){return function(a){return n(e,r,t,a)}}}}},Ky=function(n){return function(e){for(var r=e.length,t=Array(r),a=0;a<r;a++)t[a]=n(a)(e[a]);return t}};var Oi=function(n){return n.mapWithIndex},Ef={mapWithIndex:Ky,Functor0:function(){return Ce}};const Xy=function(){function n(a){return[a]}function e(a){return function(u){return[a,u]}}function r(a){return function(u){return function(o){return[a,u,o]}}}function t(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(o){return function(i){return function(c){function l(s,p){switch(p-s){case 0:return o([]);case 1:return u(n)(i(c[s]));case 2:return a(u(e)(i(c[s])))(i(c[s+1]));case 3:return a(a(u(r)(i(c[s])))(i(c[s+1])))(i(c[s+2]));default:var m=s+Math.floor((p-s)/4)*2;return a(u(t)(l(s,m)))(l(m,p))}}return l(0,c.length)}}}}}}();var Jy=An(Tn),po=function(n){return n.traverse},Zy=function(n){var e=po(n);return function(r){return e(r)(Jy)}},If={traverse:function(n){var e=n.Apply0();return Xy(ne(e))(k(e.Functor0()))(S(n))},sequence:function(n){return Zy(If)(n)},Functor0:function(){return Ce},Foldable1:function(){return he}},ng=$p(),eg=se(ma),rg=Yy(ox),tg=function(){return rg(q.create)}(),Hp=function(){return Up(ix)},ag=function(n){return function(e){return Cy(Ma(e))(n)()}},ug=function(n){return[n]},ct=Up(rx),Gl=function(){return Af(tx)($.create)(_.value)}(),og=function(){return Af(ax)($.create)(_.value)}(),ig=function(){return Af(ux)($.create)(_.value)}(),cg=function(n){return function(e){return function(r){return r.length===0?[]:pn(r)(function(t){return ng(ig(t)(r))})(og(n(e))(r))}}},ki=function(n){return function(e){return eg([n])(e)}},fg=function(n){return n.traverseWithIndex},jp=function(){function n(e,r){this.value0=e,this.value1=r}return n.create=function(e){return function(r){return new n(e,r)}},n}(),lg=function(n){var e=Ir(n);return function(r){return new jp(r,e)}},wn=function(){function n(){}return n.value=new n,n}(),E=function(){function n(e,r){this.value0=e,this.value1=r}return n.create=function(e){return function(r){return new n(e,r)}},n}(),sg=function(n){return n},vg=function(n){return new E(n.value0,n.value1)},pg=function(n){var e=function(r){return function(t){var a=r,u=!1,o;function i(c,l){if(l instanceof E&&l.value1 instanceof E&&l.value1.value1 instanceof E){a=new E(l,c),t=l.value1.value1.value1;return}var s=function(m){return m instanceof E&&m.value1 instanceof E&&m.value1.value1 instanceof wn?new E(n(m.value0),new E(n(m.value1.value0),wn.value)):m instanceof E&&m.value1 instanceof wn?new E(n(m.value0),wn.value):wn.value},p=function(m){return function(v){var f=m,d=!1,h;function b(O,g){if(O instanceof E&&O.value0 instanceof E&&O.value0.value1 instanceof E&&O.value0.value1.value1 instanceof E){f=O.value1,v=new E(n(O.value0.value0),new E(n(O.value0.value1.value0),new E(n(O.value0.value1.value1.value0),g)));return}return d=!0,g}for(;!d;)h=b(f,v);return h}};return u=!0,p(c)(s(l))}for(;!u;)o=i(a,t);return o}};return e(wn.value)},qf={map:pg},dg=k(qf),li={foldr:function(n){return function(e){var r=function(){var a=function(u){return function(o){var i=u,c=!1,l;function s(p,m){if(m instanceof wn)return c=!0,p;if(m instanceof E){i=new E(m.value0,p),o=m.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[p.constructor.name,m.constructor.name])}for(;!c;)l=s(i,o);return l}};return a(wn.value)}(),t=Pt(li)(Kn(n))(e);return function(a){return t(r(a))}}},foldl:function(n){var e=function(r){return function(t){var a=r,u=!1,o;function i(c,l){if(l instanceof wn)return u=!0,c;if(l instanceof E){a=n(c)(l.value0),t=l.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[l.constructor.name])}for(;!u;)o=i(a,t);return o}};return e},foldMap:function(n){var e=se(n.Semigroup0()),r=Xe(n);return function(t){return Pt(li)(function(a){var u=e(a);return function(o){return u(t(o))}})(r)}}},mg=Pr(li),Vp={append:function(n){return function(e){return mg(E.create)(e)(n)}}},Ff=se(Vp),hg={append:function(n){return function(e){return new jp(n.value0,Ff(n.value1)(vg(e)))}}},Gp={eq1:function(n){var e=hf(n);return function(r){return function(t){var a=function(u){return function(o){return function(i){var c=u,l=o,s=!1,p;function m(v,f,d){if(!d)return s=!0,!1;if(v instanceof wn&&f instanceof wn)return s=!0,d;if(v instanceof E&&f instanceof E){c=v.value1,l=f.value1,i=d&&e(f.value0)(v.value0);return}return s=!0,!1}for(;!s;)p=m(c,l,i);return p}}};return a(r)(t)(!0)}}}},xg=Ax(Gp),yg=function(n){return{eq:xg(n)}},gg={compare1:function(n){var e=ut(n);return function(r){return function(t){var a=function(u){return function(o){var i=u,c=!1,l;function s(p,m){if(p instanceof wn&&m instanceof wn)return c=!0,ze.value;if(p instanceof wn)return c=!0,er.value;if(m instanceof wn)return c=!0,at.value;if(p instanceof E&&m instanceof E){var v=e(p.value0)(m.value0);if(v instanceof ze){i=p.value1,o=m.value1;return}return c=!0,v}throw new Error("Failed pattern match at Data.List.Types (line 60, column 5 - line 60, column 20): "+[p.constructor.name,m.constructor.name])}for(;!c;)l=s(i,o);return l}};return a(r)(t)}}},Eq10:function(){return Gp}},wg=zx(gg),Qp=function(n){var e=yg(n.Eq0());return{compare:wg(n),Eq0:function(){return e}}},Yp={apply:function(n){return function(e){if(n instanceof wn)return wn.value;if(n instanceof E)return Ff(dg(n.value0)(e))(ne(Yp)(n.value1)(e));throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): "+[n.constructor.name,e.constructor.name])}},Functor0:function(){return qf}},bg={pure:function(n){return new E(n,wn.value)},Apply0:function(){return Yp}},$g={alt:Ff,Functor0:function(){return qf}},_g=function(){return{empty:wn.value,Alt0:function(){return $g}}}(),Og=Pr(li),kg=function(n){return function(e){return Og(E.create)(new E(e,wn.value))(n)}};const Sg=function(n){return n()},Tg=function(n){throw new Error(n)};var Pg=function(){return Tg},Ag=Pg(),Eg=Sg,Wa=function(n){return Eg(function(){return Ag(n)})},C=function(){function n(){}return n.value=new n,n}(),W=function(){function n(e,r,t,a){this.value0=e,this.value1=r,this.value2=t,this.value3=a}return n.create=function(e){return function(r){return function(t){return function(a){return new n(e,r,t,a)}}}},n}(),sn=function(){function n(e,r,t,a,u,o,i){this.value0=e,this.value1=r,this.value2=t,this.value3=a,this.value4=u,this.value5=o,this.value6=i}return n.create=function(e){return function(r){return function(t){return function(a){return function(u){return function(o){return function(i){return new n(e,r,t,a,u,o,i)}}}}}}},n}(),At=function(){function n(e,r,t){this.value0=e,this.value1=r,this.value2=t}return n.create=function(e){return function(r){return function(t){return new n(e,r,t)}}},n}(),rt=function(){function n(e,r,t){this.value0=e,this.value1=r,this.value2=t}return n.create=function(e){return function(r){return function(t){return new n(e,r,t)}}},n}(),Et=function(){function n(e,r,t,a,u,o){this.value0=e,this.value1=r,this.value2=t,this.value3=a,this.value4=u,this.value5=o}return n.create=function(e){return function(r){return function(t){return function(a){return function(u){return function(o){return new n(e,r,t,a,u,o)}}}}}},n}(),Nr=function(){function n(e,r,t,a,u,o){this.value0=e,this.value1=r,this.value2=t,this.value3=a,this.value4=u,this.value5=o}return n.create=function(e){return function(r){return function(t){return function(a){return function(u){return function(o){return new n(e,r,t,a,u,o)}}}}}},n}(),It=function(){function n(e,r,t,a,u,o){this.value0=e,this.value1=r,this.value2=t,this.value3=a,this.value4=u,this.value5=o}return n.create=function(e){return function(r){return function(t){return function(a){return function(u){return function(o){return new n(e,r,t,a,u,o)}}}}}},n}(),Po=function(){function n(e,r,t,a){this.value0=e,this.value1=r,this.value2=t,this.value3=a}return n.create=function(e){return function(r){return function(t){return function(a){return new n(e,r,t,a)}}}},n}(),Kp=function(n){var e=ut(n);return function(r){var t=function(a){var u=!1,o;function i(c){if(c instanceof C)return u=!0,_.value;if(c instanceof W){var l=e(r)(c.value1);if(l instanceof ze)return u=!0,new $(c.value2);if(l instanceof er){a=c.value0;return}a=c.value3;return}if(c instanceof sn){var s=e(r)(c.value1);if(s instanceof ze)return u=!0,new $(c.value2);var p=e(r)(c.value4);if(p instanceof ze)return u=!0,new $(c.value5);if(s instanceof er){a=c.value0;return}if(p instanceof at){a=c.value6;return}a=c.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[c.constructor.name])}for(;!u;)o=i(a);return o};return t}},Ig=function(n){return n instanceof C},Xp=function(n){return function(e){return function(r){var t=n,a=e,u=!1,o;function i(c,l,s){if(l instanceof wn)return u=!0,s;if(l instanceof E){if(l.value0 instanceof At){t=c,a=l.value1,r=new W(s,l.value0.value0,l.value0.value1,l.value0.value2);return}if(l.value0 instanceof rt){t=c,a=l.value1,r=new W(l.value0.value0,l.value0.value1,l.value0.value2,s);return}if(l.value0 instanceof Et){t=c,a=l.value1,r=new sn(s,l.value0.value0,l.value0.value1,l.value0.value2,l.value0.value3,l.value0.value4,l.value0.value5);return}if(l.value0 instanceof Nr){t=c,a=l.value1,r=new sn(l.value0.value0,l.value0.value1,l.value0.value2,s,l.value0.value3,l.value0.value4,l.value0.value5);return}if(l.value0 instanceof It){t=c,a=l.value1,r=new sn(l.value0.value0,l.value0.value1,l.value0.value2,l.value0.value3,l.value0.value4,l.value0.value5,s);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[l.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[l.constructor.name,s.constructor.name])}for(;!u;)o=i(t,a,r);return o}}},Jp=function(n){var e=Xp(n),r=ut(n);return function(t){return function(a){var u=function(i){return function(c){var l=i,s=!1,p;function m(v,f){if(v instanceof wn)return s=!0,new W(f.value0,f.value1,f.value2,f.value3);if(v instanceof E){if(v.value0 instanceof At)return s=!0,e(v.value1)(new sn(f.value0,f.value1,f.value2,f.value3,v.value0.value0,v.value0.value1,v.value0.value2));if(v.value0 instanceof rt)return s=!0,e(v.value1)(new sn(v.value0.value0,v.value0.value1,v.value0.value2,f.value0,f.value1,f.value2,f.value3));if(v.value0 instanceof Et){l=v.value1,c=new Po(new W(f.value0,f.value1,f.value2,f.value3),v.value0.value0,v.value0.value1,new W(v.value0.value2,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof Nr){l=v.value1,c=new Po(new W(v.value0.value0,v.value0.value1,v.value0.value2,f.value0),f.value1,f.value2,new W(f.value3,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof It){l=v.value1,c=new Po(new W(v.value0.value0,v.value0.value1,v.value0.value2,v.value0.value3),v.value0.value4,v.value0.value5,new W(f.value0,f.value1,f.value2,f.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[v.value0.constructor.name,f.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[v.constructor.name,f.constructor.name])}for(;!s;)p=m(l,c);return p}},o=function(i){return function(c){var l=i,s=!1,p;function m(v,f){if(f instanceof C)return s=!0,u(v)(new Po(C.value,t,a,C.value));if(f instanceof W){var d=r(t)(f.value1);if(d instanceof ze)return s=!0,e(v)(new W(f.value0,t,a,f.value3));if(d instanceof er){l=new E(new At(f.value1,f.value2,f.value3),v),c=f.value0;return}l=new E(new rt(f.value0,f.value1,f.value2),v),c=f.value3;return}if(f instanceof sn){var d=r(t)(f.value1);if(d instanceof ze)return s=!0,e(v)(new sn(f.value0,t,a,f.value3,f.value4,f.value5,f.value6));var h=r(t)(f.value4);if(h instanceof ze)return s=!0,e(v)(new sn(f.value0,f.value1,f.value2,f.value3,t,a,f.value6));if(d instanceof er){l=new E(new Et(f.value1,f.value2,f.value3,f.value4,f.value5,f.value6),v),c=f.value0;return}if(d instanceof at&&h instanceof er){l=new E(new Nr(f.value0,f.value1,f.value2,f.value4,f.value5,f.value6),v),c=f.value3;return}l=new E(new It(f.value0,f.value1,f.value2,f.value3,f.value4,f.value5),v),c=f.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[v.constructor.name,f.constructor.name])}for(;!s;)p=m(l,c);return p}};return o(wn.value)}}},qg=function(n){var e=Xp(n),r=ut(n);return function(t){var a=function(c){return function(l){var s=c,p=!1,m;function v(f,d){if(f instanceof wn)return p=!0,d;if(f instanceof E){if(f.value0 instanceof At&&f.value0.value2 instanceof C&&d instanceof C)return p=!0,e(f.value1)(new W(C.value,f.value0.value0,f.value0.value1,C.value));if(f.value0 instanceof rt&&f.value0.value0 instanceof C&&d instanceof C)return p=!0,e(f.value1)(new W(C.value,f.value0.value1,f.value0.value2,C.value));if(f.value0 instanceof At&&f.value0.value2 instanceof W){s=f.value1,l=new sn(d,f.value0.value0,f.value0.value1,f.value0.value2.value0,f.value0.value2.value1,f.value0.value2.value2,f.value0.value2.value3);return}if(f.value0 instanceof rt&&f.value0.value0 instanceof W){s=f.value1,l=new sn(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3,f.value0.value1,f.value0.value2,d);return}return f.value0 instanceof At&&f.value0.value2 instanceof sn?(p=!0,e(f.value1)(new W(new W(d,f.value0.value0,f.value0.value1,f.value0.value2.value0),f.value0.value2.value1,f.value0.value2.value2,new W(f.value0.value2.value3,f.value0.value2.value4,f.value0.value2.value5,f.value0.value2.value6)))):f.value0 instanceof rt&&f.value0.value0 instanceof sn?(p=!0,e(f.value1)(new W(new W(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3),f.value0.value0.value4,f.value0.value0.value5,new W(f.value0.value0.value6,f.value0.value1,f.value0.value2,d)))):f.value0 instanceof Et&&f.value0.value2 instanceof C&&f.value0.value5 instanceof C&&d instanceof C?(p=!0,e(f.value1)(new sn(C.value,f.value0.value0,f.value0.value1,C.value,f.value0.value3,f.value0.value4,C.value))):f.value0 instanceof Nr&&f.value0.value0 instanceof C&&f.value0.value5 instanceof C&&d instanceof C?(p=!0,e(f.value1)(new sn(C.value,f.value0.value1,f.value0.value2,C.value,f.value0.value3,f.value0.value4,C.value))):f.value0 instanceof It&&f.value0.value0 instanceof C&&f.value0.value3 instanceof C&&d instanceof C?(p=!0,e(f.value1)(new sn(C.value,f.value0.value1,f.value0.value2,C.value,f.value0.value4,f.value0.value5,C.value))):f.value0 instanceof Et&&f.value0.value2 instanceof W?(p=!0,e(f.value1)(new W(new sn(d,f.value0.value0,f.value0.value1,f.value0.value2.value0,f.value0.value2.value1,f.value0.value2.value2,f.value0.value2.value3),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Nr&&f.value0.value0 instanceof W?(p=!0,e(f.value1)(new W(new sn(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3,f.value0.value1,f.value0.value2,d),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Nr&&f.value0.value5 instanceof W?(p=!0,e(f.value1)(new W(f.value0.value0,f.value0.value1,f.value0.value2,new sn(d,f.value0.value3,f.value0.value4,f.value0.value5.value0,f.value0.value5.value1,f.value0.value5.value2,f.value0.value5.value3)))):f.value0 instanceof It&&f.value0.value3 instanceof W?(p=!0,e(f.value1)(new W(f.value0.value0,f.value0.value1,f.value0.value2,new sn(f.value0.value3.value0,f.value0.value3.value1,f.value0.value3.value2,f.value0.value3.value3,f.value0.value4,f.value0.value5,d)))):f.value0 instanceof Et&&f.value0.value2 instanceof sn?(p=!0,e(f.value1)(new sn(new W(d,f.value0.value0,f.value0.value1,f.value0.value2.value0),f.value0.value2.value1,f.value0.value2.value2,new W(f.value0.value2.value3,f.value0.value2.value4,f.value0.value2.value5,f.value0.value2.value6),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Nr&&f.value0.value0 instanceof sn?(p=!0,e(f.value1)(new sn(new W(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3),f.value0.value0.value4,f.value0.value0.value5,new W(f.value0.value0.value6,f.value0.value1,f.value0.value2,d),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Nr&&f.value0.value5 instanceof sn?(p=!0,e(f.value1)(new sn(f.value0.value0,f.value0.value1,f.value0.value2,new W(d,f.value0.value3,f.value0.value4,f.value0.value5.value0),f.value0.value5.value1,f.value0.value5.value2,new W(f.value0.value5.value3,f.value0.value5.value4,f.value0.value5.value5,f.value0.value5.value6)))):f.value0 instanceof It&&f.value0.value3 instanceof sn?(p=!0,e(f.value1)(new sn(f.value0.value0,f.value0.value1,f.value0.value2,new W(f.value0.value3.value0,f.value0.value3.value1,f.value0.value3.value2,f.value0.value3.value3),f.value0.value3.value4,f.value0.value3.value5,new W(f.value0.value3.value6,f.value0.value4,f.value0.value5,d)))):(p=!0,Wa("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[f.constructor.name])}for(;!p;)m=v(s,l);return m}},u=function(c){return function(l){var s=c,p=!1,m;function v(f,d){if(d instanceof W&&d.value0 instanceof C&&d.value3 instanceof C)return p=!0,a(f)(C.value);if(d instanceof W){s=new E(new rt(d.value0,d.value1,d.value2),f),l=d.value3;return}if(d instanceof sn&&d.value0 instanceof C&&d.value3 instanceof C&&d.value6 instanceof C)return p=!0,a(new E(new rt(C.value,d.value1,d.value2),f))(C.value);if(d instanceof sn){s=new E(new It(d.value0,d.value1,d.value2,d.value3,d.value4,d.value5),f),l=d.value6;return}return p=!0,Wa("The impossible happened in partial function `removeMaxNode`.")}for(;!p;)m=v(s,l);return m}},o=function(c){var l=!1,s;function p(m){if(m instanceof W&&m.value3 instanceof C)return l=!0,{key:m.value1,value:m.value2};if(m instanceof W){c=m.value3;return}if(m instanceof sn&&m.value6 instanceof C)return l=!0,{key:m.value4,value:m.value5};if(m instanceof sn){c=m.value6;return}return l=!0,Wa("The impossible happened in partial function `maxNode`.")}for(;!l;)s=p(c);return s},i=function(c){return function(l){var s=c,p=!1,m;function v(f,d){if(d instanceof C)return p=!0,_.value;if(d instanceof W){var h=r(t)(d.value1);if(d.value3 instanceof C&&h instanceof ze)return p=!0,new $(new q(d.value2,a(f)(C.value)));if(h instanceof ze){var b=o(d.value0);return p=!0,new $(new q(d.value2,u(new E(new At(b.key,b.value,d.value3),f))(d.value0)))}if(h instanceof er){s=new E(new At(d.value1,d.value2,d.value3),f),l=d.value0;return}s=new E(new rt(d.value0,d.value1,d.value2),f),l=d.value3;return}if(d instanceof sn){var O=function(){return d.value0 instanceof C&&d.value3 instanceof C&&d.value6 instanceof C}(),h=r(t)(d.value4),g=r(t)(d.value1);if(O&&g instanceof ze)return p=!0,new $(new q(d.value2,e(f)(new W(C.value,d.value4,d.value5,C.value))));if(O&&h instanceof ze)return p=!0,new $(new q(d.value5,e(f)(new W(C.value,d.value1,d.value2,C.value))));if(g instanceof ze){var b=o(d.value0);return p=!0,new $(new q(d.value2,u(new E(new Et(b.key,b.value,d.value3,d.value4,d.value5,d.value6),f))(d.value0)))}if(h instanceof ze){var b=o(d.value3);return p=!0,new $(new q(d.value5,u(new E(new Nr(d.value0,d.value1,d.value2,b.key,b.value,d.value6),f))(d.value3)))}if(g instanceof er){s=new E(new Et(d.value1,d.value2,d.value3,d.value4,d.value5,d.value6),f),l=d.value0;return}if(g instanceof at&&h instanceof er){s=new E(new Nr(d.value0,d.value1,d.value2,d.value4,d.value5,d.value6),f),l=d.value3;return}s=new E(new It(d.value0,d.value1,d.value2,d.value3,d.value4,d.value5),f),l=d.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[d.constructor.name])}for(;!p;)m=v(s,l);return m}};return i(wn.value)}},Ue={foldr:function(n){return function(e){return function(r){if(r instanceof C)return e;if(r instanceof W)return Pr(Ue)(n)(n(r.value2)(Pr(Ue)(n)(e)(r.value3)))(r.value0);if(r instanceof sn)return Pr(Ue)(n)(n(r.value2)(Pr(Ue)(n)(n(r.value5)(Pr(Ue)(n)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(n){return function(e){return function(r){if(r instanceof C)return e;if(r instanceof W)return Pt(Ue)(n)(n(Pt(Ue)(n)(e)(r.value0))(r.value2))(r.value3);if(r instanceof sn)return Pt(Ue)(n)(n(Pt(Ue)(n)(n(Pt(Ue)(n)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(n){var e=Xe(n),r=se(n.Semigroup0());return function(t){return function(a){if(a instanceof C)return e;if(a instanceof W)return r(yu(Ue)(n)(t)(a.value0))(r(t(a.value2))(yu(Ue)(n)(t)(a.value3)));if(a instanceof sn)return r(yu(Ue)(n)(t)(a.value0))(r(t(a.value2))(r(yu(Ue)(n)(t)(a.value3))(r(t(a.value5))(yu(Ue)(n)(t)(a.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[a.constructor.name])}}}},Fg=function(n){var e=Wx(n),r=Lx(n),t=Ux(n);return function(a){return function(u){return function(o){return function(i){return function(c){var l=function(){if(o instanceof $)return function(v){return e(v)(o.value0)};if(o instanceof _)return Fn(!1);throw new Error("Failed pattern match at Data.Map.Internal (line 363, column 7 - line 367, column 22): "+[o.constructor.name])}(),s=function(){if(i instanceof $)return function(v){return r(v)(i.value0)};if(i instanceof _)return Fn(!1);throw new Error("Failed pattern match at Data.Map.Internal (line 370, column 7 - line 374, column 22): "+[i.constructor.name])}(),p=function(){if(o instanceof $&&i instanceof $)return function(v){return t(o.value0)(v)&&t(v)(i.value0)};if(o instanceof $&&i instanceof _)return function(v){return t(o.value0)(v)};if(o instanceof _&&i instanceof $)return function(v){return t(v)(i.value0)};if(o instanceof _&&i instanceof _)return Fn(!0);throw new Error("Failed pattern match at Data.Map.Internal (line 377, column 7 - line 385, column 21): "+[o.constructor.name,i.constructor.name])}(),m=function(v){if(v instanceof C)return u;if(v instanceof W)return a(a(function(){var f=l(v.value1);return f?u:m(v.value0)}())(function(){var f=p(v.value1);return f?c(v.value1)(v.value2):u}()))(function(){var f=s(v.value1);return f?u:m(v.value3)}());if(v instanceof sn)return a(a(a(a(function(){var f=l(v.value1);return f?u:m(v.value0)}())(function(){var f=p(v.value1);return f?c(v.value1)(v.value2):u}()))(function(){var f=l(v.value4)||s(v.value1);return f?u:m(v.value3)}()))(function(){var f=p(v.value4);return f?c(v.value4)(v.value5):u}()))(function(){var f=s(v.value4);return f?u:m(v.value6)}());throw new Error("Failed pattern match at Data.Map.Internal (line 403, column 10 - line 415, column 67): "+[v.constructor.name])};return m}}}}}},Zp=function(n){var e=Fg(n);return function(r){return e(se(r.Semigroup0()))(Xe(r))}},Ja=function(){return C.value}(),Si=function(n){var e=qg(n);return function(r){return function(t){return pn(t)(Xa)(e(r)(t))}}},Cf=function(n){var e=Kp(n),r=Si(n),t=Jp(n);return function(a){return function(u){return function(o){var i=a(e(u)(o));if(i instanceof _)return r(u)(o);if(i instanceof $)return t(u)(i.value0)(o);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[i.constructor.name])}}}},Ql=Pe(rr),Cg=S(Le),Bg=ne(_i),Dg=k(rr),Rg={compact:function(n){return function(){var r=ju(),t=jl(function(a){return Gl(n)(a)})();return Vl(t)(function(a){return Ql(function(u){if(u instanceof _)return Cg(0);if(u instanceof $)return Ma(u.value0)(r);throw new Error("Failed pattern match at Data.Compactable (line 111, column 34 - line 113, column 35): "+[u.constructor.name])}(a))})(),ni(r)()}()},separate:function(n){return function(){var r=ju(),t=ju(),a=jl(function(u){return Gl(n)(u)})();return Vl(a)(function(u){return Ql(function(o){if(o instanceof Qn)return Ma(o.value0)(r);if(o instanceof Ln)return Ma(o.value0)(t);throw new Error("Failed pattern match at Data.Compactable (line 122, column 34 - line 124, column 31): "+[o.constructor.name])}(u))})(),Bg(Dg(function(u){return function(o){return{left:u,right:o}}})(ni(r)))(ni(t))()}()}},n0=function(n){return n.compact},e0=function(n){return n.partitionMap},Ng=function(n){return function(e){var r=n(e);return r?new $(e):_.value}},Ct=function(n){return n.filterMap},zg=function(n){return n.filter},Lg=function(n){return function(e){var r=n(e);return r?new Ln(e):new Qn(e)}};function Mg(n){return()=>n.slice()}function Wg(n){return e=>r=>()=>{r[n]=e}}function Ug(n){return()=>n.slice()}const r0=n=>{for(var e=0,r=n.length;e<r;e++)n[e]()},Hg=(n,e)=>{for(var r=0,t=n.length;r<t;r++)e(n[r])},t0=()=>({r:!1,q:[],m:[{}]}),a0=(n,e,r)=>{r.m[r.m.length-1][n]=e},u0=(n,e)=>{for(const r of e.m)if(delete r[n])return!0;return!1},Bf=(n,e)=>{if(n.r){n.q.push(()=>{Bf(n,e)});return}n.r=!0;const r={},t=u=>{n.m.push({});for(const o of Object.entries(n.m[u])){const i=o[0],c=o[1];e(c),Object.keys(n.m[u+1]).length&&t(u+1),n.m[u+1]={},n.m.length=u+1+1,r[i]=c}};t(0),n.m.length=0,n.m.push(r);let a;for(n.r=!1;a=n.q.shift();)a()};var jg=qr,Ti={liftST:An(Tn),Monad0:function(){return Dp}},Je={liftST:jg,Monad0:function(){return so}},Ie=function(n){return n.liftST};function Vg(n){return function(e){return function(){return setTimeout(e,n)}}}function Gg(n){return function(){clearTimeout(n)}}var o0=Vg,Yl=Gg;const Pi=function(e){return function(r){return e(r)()}},Kl=function(e){return function(r){return function(){return e(r)}}};var Qg=function(n){var e=se(Bp(n));return{append:function(r){return function(t){return Pi(function(a){return e(Kl(r)(a))(Kl(t)(a))})}}}},Yg=function(n){var e=Xe(vy(n)),r=Qg(n.Semigroup0());return{mempty:Pi(function(t){return e}),Semigroup0:function(){return r}}},Kg=An(Tn),Xg=k(Mp),Jg=S(Op),$a=function(n){return n.sampleOnRight},Zg=function(n){var e=$a(n),r=k(n.Filterable2().Functor1());return function(t){return function(a){return e(t)(r(oo)(a))}}},nw=function(n){return n.sampleOnLeft},zt=function(n){return n.once},Ai=function(n){return n.keepLatest},Ei=function(n){return n.fix},Za=function(n){var e=Ei(n),r=$a(n),t=ve(n.Alt1()),a=n.Filterable2().Functor1(),u=Ae(a),o=zt(n),i=k(a);return function(c){return function(l){return function(s){return e(function(p){return r(t(p)(u(o(s))(l)))(i(Kn(c))(s))})}}}},Df=function(n){var e=Ct(n.Filterable2()),r=Za(n);return function(t){return function(a){return function(u){return e(Xa)(r(function(o){return function(i){return Xg(Jg)(t(o.value0)(i))}})(new q(a,_.value))(u))}}}},ew=function(n){var e=Ct(n.Filterable2()),r=Za(n);return function(t){var a=function(u){return function(o){if(u instanceof _)return new $({now:o,last:_.value});if(u instanceof $)return new $({now:o,last:new $(u.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 83, column 3 - line 83, column 50): "+[u.constructor.name,o.constructor.name])}};return e(Kg)(r(a)(_.value)(t))}};function rw(n){return function(e){return n===e}}var tw=rw,Ku=Pe(Ee),ke=Ie(Je),si=vr(gn)(tr),Ii=S(Le),Er=Pe(rr),aw=Dt(vo),Va=Ie(Ti),uw=se(ma),i0=Xe(Yg(Pp)),ow=An(Tn),Xl=k(io),iw=ne(_i),cw=k(rr),sr=function(n){return function(e){return function(){return n(!1,Pi(e))}}},fw=function(n){return function(e){return function(r,t){var a=J(_.value)(),u=n(r,function(i){return Ku(ke(Yn(new $(i))(a)))()}),o=e(r,function(i){var c=ke(V(a))();return si(c)(function(l){return function(){return t(i(l))}})()});return function(){return u(),o()}}}},lw=function(n){return function(e){return function(r,t){var a=J(_.value)(),u=n(r,function(i){var c=ke(V(a))();return si(c)(function(l){return function(){return t(l(i))}})()}),o=e(r,function(i){return ke(Er(Yn(new $(i))(a)))()});return function(){return u(),o()}}}},sw=function(n){return function(e,r){var t=J(_.value)(),a=J(Ii(w))(),u=n(e,function(i){var c=ke(V(t))();if(c instanceof _)return Ku(ke(Yn(new $(i))(t)))(),r(i),ke(aw(V(a)))();if(c instanceof $)return w;throw new Error("Failed pattern match at FRP.Event (line 185, column 9 - line 191, column 30): "+[c.constructor.name])});Er(Yn(u)(a))();var o=Va(V(t))();return function(){return o instanceof $?u():w}(),u}},vw=function(n){return function(e){return function(r,t){var a=ju();return $i(e)(function(u){var o=n(u);return function(){var c=o(r,t);return Er(Va(Ma(c)(a)))()}})(),function(){var o=Va(Np(a))();return r0(o)}}}},Rf=function(n){return function(e,r){var t=ju();return $i(n)(function(a){return function(){var o=a(e,r);return Er(Va(Ma(o)(t)))()}})(),function(){var u=Va(Np(t))();return r0(u)}}},pw=function(n){return function(e,r){var t=function(a,u){return a(e,u)};return n(t,r)}},Ga=function(n){return function(e,r){var t=function(a){return function(u){return function(){return a(e,Pi(u))}}};return n(t)(function(a){return function(){return r(a)}})()}},nu=function(n){return function(e,r){return e?Ii(w):n(function(t){return function(){return r(t)}})()}},dw=function(n){var e=Cf(n),r=Kp(n);return function(){var a=J(Ja)();return{event:function(u){return function(o,i){return Er(ie(e(function(c){if(c instanceof _)return new $([i]);if(c instanceof $)return new $(uw(c.value0)([i]));throw new Error("Failed pattern match at FRP.Event (line 547, column 17 - line 549, column 51): "+[c.constructor.name])})(u))(a))(),Er(ie(e(function(c){if(c instanceof _)return _.value;if(c instanceof $)return new $(cg(tw)(i)(c.value0));throw new Error("Failed pattern match at FRP.Event (line 556, column 17 - line 558, column 65): "+[c.constructor.name])})(u))(a))}},push:function(u){var o=ke(V(a))(),i=r(u.address)(o);if(i instanceof _)return w;if(i instanceof $)return Hg(i.value0,function(c){return c(u.payload)});throw new Error("Failed pattern match at FRP.Event (line 565, column 9 - line 567, column 95): "+[i.constructor.name])}}}},mw=function(n){return function(){var r=dw(n)();return{push:function(t){return function(){return r.push(t)}},event:r.event}}},hw=function(n){return function(e,r){var t=J(Ii(w))(),a=n(e,function(u){return ke(function(){var i=V(t)();i();var c=u(e,r);return Er(Va(Yn(c)(t)))()})()});return function(){var o=V(t)();return o(),a()}}},U={map:function(n){return function(e){return function(r,t){return e(r,function(a){return t(n(a))})}}}},c0=k(U),Vu=function(n){return function(e){return function(r,t){return e(r,function(a){var u=n(a);if(u instanceof $)return t(u.value0);if(u instanceof _)return w;throw new Error("Failed pattern match at FRP.Event (line 206, column 31 - line 208, column 35): "+[u.constructor.name])})}}},fc=function(n){return Vu(function(e){var r=n(e);if(r)return new $(e);if(!r)return _.value;throw new Error("Failed pattern match at FRP.Event (line 112, column 13 - line 114, column 25): "+[r.constructor.name])})},f0=function(n){return function(e){return function(r,t){return e(r,function(a){var u=ke(J(_.value))(),o=o0(n)(function(){var c=ke(V(u))();return t(new Ln(new q(c,a)))})();return Ku(ke(Yn(new $(o))(u)))(),t(new Qn(o))})}}},l0=function(){var e=t0(),r=J(0)();return{event:function(t,a){var u=J(a)(),o=V(r)();return a0(o,u,e),Er(ie(function(i){return i+1|0})(r))(),function(){return Er(Yn(i0)(u))(),u0(o,e),w}},push:function(t){return function(){return Bf(e,function(a){var u=ke(V(a))();return u(t)})}}}},s0=l0,xw=function(){var e=t0(),r=J(0)();return{event:function(t,a){var u=J(a)(),o=V(r)();return a0(o,u,e),Er(ie(function(i){return i+1|0})(r))(),function(){return Er(Yn(i0)(u))(),u0(o,e),w}},push:function(t){return Bf(e,function(a){var u=ke(V(a))();return u(t)})}}},yw=function(n){return function(e,r){var t=xw(),a=n(t.event),u=t.event(e,r),o=a(e,t.push);return function(){return o(),u()}}},_a=l0,v0=function(n){return function(){var r=ke(_a)(),t=n(r.push)();return{event:r.event,unsubscribe:t}}},eu={compact:Vu(ow),separate:function(n){return{left:Vu(function(e){if(e instanceof Qn)return new $(e.value0);if(e instanceof Ln)return _.value;throw new Error("Failed pattern match at FRP.Event (line 95, column 13 - line 97, column 33): "+[e.constructor.name])})(n),right:Vu(function(e){if(e instanceof Ln)return new $(e.value0);if(e instanceof Qn)return _.value;throw new Error("Failed pattern match at FRP.Event (line 102, column 13 - line 104, column 32): "+[e.constructor.name])})(n)}}},vi={filter:fc,filterMap:Vu,partition:function(n){return function(e){return{yes:fc(n)(e),no:fc(function(r){return!n(r)})(e)}}},partitionMap:function(n){return function(e){return{left:Ct(vi)(function(){var r=Ha($.create)(Fn(_.value));return function(t){return r(n(t))}}())(e),right:Ct(vi)(function(r){return wf(n(r))})(e)}}},Compactable0:function(){return eu},Functor1:function(){return U}},Qa=Xl(Xl(function(){var n=c0(Xa),e=Ct(vi)(wf);return function(r){return n(e(r))}}()))(f0),gw=function(n){return function(e){return function(r,t){var a=J(_.value)(),u=J(_.value)(),o=n(r,function(c){Ku(ke(Yn(new $(c))(a)))();var l=ke(V(u))();return si(l)(function(s){return function(){return t(s(c))}})()}),i=e(r,function(c){Ku(ke(Yn(new $(c))(u)))();var l=ke(V(a))();return si(l)(function(s){return function(){return t(c(s))}})()});return function(){return o(),i()}}}},Z={apply:function(n){return function(e){return gw(n)(c0(oo)(e))}},Functor0:function(){return U}},He={alt:function(n){return function(e){return function(r,t){return iw(cw(function(a){return function(u){return function(){return a(),u()}}})(function(){return n(r,t)}))(function(){return e(r,t)})()}}},Functor0:function(){return U}},xe={empty:function(n,e){return Ii(w)},Alt0:function(){return He}},ye={keepLatest:hw,sampleOnRight:fw,sampleOnLeft:lw,fix:yw,once:sw,Plus0:function(){return xe},Alt1:function(){return He},Filterable2:function(){return vi}};const qi=function(){return window};function ww(n,e,r,t){if(typeof window<"u"){var a=window[r];if(a!=null&&t instanceof a)return e(t)}for(var u=t;u!=null;){var o=Object.getPrototypeOf(u),i=o.constructor.name;if(i===r)return e(t);if(i==="Object")return n;u=o}return n}var Nf=function(n){return function(e){return ww(_.value,$.create,n,e)}};function bw(n,e,r){return n==null?e:r(n)}var p0=function(n){return bw(n,_.value,$.create)},Jl=Nf("HTMLCanvasElement");function $w(n){return n.body}var _w=k(Ee),Ow=function(n){return _w(p0)(function(){return $w(n)})},kw=qr;function zf(n){return function(){return n.valueAsNumber}}var Lf=Nf("HTMLInputElement");function Sw(n){return function(){return n.document}}function Tw(n){return function(e){return function(){return e.requestAnimationFrame(n)}}}var d0=qr,Pw=function(n,e,r){var t=0,a;return function(u){if(t===2)return a;if(t===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return t=1,a=r(),t=2,a}},Aw=Pe(Ee),Ew=wx(so),Fi=v0(function(n){return function(){var r=qi(),t=Qe(!1)(),a=Pw("loop","FRP.Event.AnimationFrame",function(){return Aw(Tw(function(){return n(w)(),Ew(va(t))(a(26))()})(r))}),u=a(23);return u(),ja(!0)(t)}}),m0=S(Le),Iw=k(io),pi=An(Tn),qw=ot(he),Fw=Ir(Xx),Gu=Pe(rr),h0=Dt(vo),x0=Ya(Le),y0=zt(ye),Cw=Zg(ye),g0=ip(gi),Bw=k(ya),Dw=Ie(Ti),Rw=vr(Le)(he),Nw=function(n){return n},Zl=function(){function n(e,r){this.value0=e,this.value1=r}return n.create=function(e){return function(r){return new n(e,r)}},n}(),ns=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),je=function(n){return{sample:function(e){return function(r){return e(r)}}}},w0=je(),zw=function(n){var e=$a(n),r=ve(n.Alt1()),t=Ae(n.Filterable2().Functor1()),a=zt(n);return function(u){return function(o){return function(i){return e(r(t(a(i))(u))(o))(i)}}}},Be=function(n){return n.sample},Lw=Be(w0),Xn=Nw,mo=function(n){var e=Ai(n),r=k(n.Filterable2().Functor1());return function(t){return Xn(function(a){return e(r(function(u){return r(u)(t)})(a))})}},b0=mo(ye),Mw=function(n){var e=Be(n);return function(r){var t=Ai(r),a=ve(r.Alt1()),u=r.Filterable2().Functor1(),o=Ae(u),i=zt(r),c=k(u);return function(l){return function(s){return Xn(function(p){return t(a(o(i(p))(e(l)(p)))(c(function(m){return e(m)(p)})(s)))})}}}},Ww=function(n){var e=Be(n);return function(r){var t=zt(r);return function(a){return Xn(function(u){return t(e(a)(u))})}}},$0=function(n){return function(e){return vw(Kn(Lw)(e))(n)}},Uw=function(n){return function(){var r=mw(n)();return{poll:Iw(b0)(r.event),push:r.push}}},Hn=function(n){var e=k(n);return{map:function(r){return function(t){return function(a){return t(e(function(u){return function(o){return u(r(o))}})(a))}}}}},Kr=function(n){var e=Be(n);return function(r){var t=k(Hn(r));return function(a){var u=k(a);return function(o){return function(i){return function(c){return e(t(o)(i))(u(oo)(c))}}}}}},Hw=function(n){var e=Ct(n),r=n.Functor1();return function(t){var a=Ei(t),u=qw(t.Plus0()),o=Ai(t),i=zt(t);return function(c){var l=Kr(c)(r)(r);return function(s){return function(p){return e(function(m){return m instanceof ns?new $(m.value0):_.value})(a(function(m){return u([l(Zl.create)(s)(p),o(Kn(e)(m)(function(v){return v instanceof Zl?new $(l(function(f){return function(d){return new ns(v.value1(f))}})(v.value0)(i(m))):Fw}))])}))}}}}},Oa=function(n){var e=Kr(n);return function(r){var t=e(r);return function(a){return t(a)(Fn)}}},_0=Oa(w0)(U)(U),ho=function(n){return function(){var r=s0(),t=J(!1)(),a=J(m0(w))();return{unsubscribe:h0(V(a)),poll:Xn(function(u){return Ga(function(o){return function(i){return function(){var l=V(t)();x0(!l)(function(){var m=o(_0(n)(y0(u)))(r.push)();return Gu(Yn(!0)(t))(),Gu(Kn(Yn)(a)(m))()})();var s=o(Cw(u)(r.event))(i)();return s}}})})}}},jw=function(n){var e=Oa(n),r=Kr(n);return function(t){var a=nw(t),u=t.Filterable2().Functor1(),o=e(u)(u),i=r(u)(u);return function(c){return function(l){return Xn(function(s){return a(o(c)(s))(i(g0)(l)(s))})}}}},O0=function(n){var e=Oa(n),r=Kr(n);return function(t){var a=$a(t),u=t.Filterable2().Functor1(),o=e(u)(u),i=r(u)(u);return function(c){return function(l){return Xn(function(s){return a(o(c)(s))(i(g0)(l)(s))})}}}},k0=function(n){return function(e){var r=Be(e),t=O0(e)(n);return{sample:function(a){return function(u){return Xn(function(o){return r(t(a)(u))(o)})}}}}},Ve=function(n){var e=ve(n),r=Hn(n.Functor0());return{alt:function(t){return function(a){return function(u){return e(t(u))(a(u))}}},Functor0:function(){return r}}},gr=function(n){var e=Ir(n),r=Ve(n.Alt0());return{empty:function(t){return e},Alt0:function(){return r}}},Vw=function(n){var e=Kr(n);return function(r){var t=r.Filterable2().Functor1(),a=k(t),u=Ei(r),o=e(t)(t),i=mo(r);return function(c){return Xn(function(l){return a(function(s){return s.value1(s.value0)})(u(function(s){return o(q.create)(c(i(a(Yu)(s))))(l)}))})}}},Mf=function(n){var e=n0(n);return function(r){var t=Kr(r);return function(a){var u=t(a)(a);return function(o){return function(i){return Xn(function(c){return e(u(function(l){return function(s){return Bw(s)(o(l))}})(i)(c))})}}}}},S0=function(n){return function(e){var r=Mf(e)(n);return function(t){var a=k(Hn(t)),u=r(t);return function(o){return function(i){var c=a(o)(i);return{left:u(Ha($.create)(Fn(_.value)))(c),right:u(Ha(Fn(_.value))($.create))(c)}}}}}},Gw=function(n){return function(e){var r=Mf(e);return function(t){return{compact:r(t)(n)(pi),separate:S0(t)(e)(n)(pi)}}}},ru=function(n){var e=Gw(n),r=Hn(n);return function(t){var a=Mf(t),u=e(t);return function(o){var i=a(o)(n),c=S0(o)(t)(n),l=u(o);return{filterMap:i,filter:function(s){return i(Ng(s))},partitionMap:c,partition:function(s){return function(p){var m=c(Lg(s))(p);return{no:m.left,yes:m.right}}},Compactable0:function(){return l},Functor1:function(){return r}}}}},tu=function(n){var e=n.Filterable2(),r=Hw(e)(n),t=gr(n.Plus0()),a=Ve(n.Alt1()),u=ru(e.Functor1())(e.Compactable0());return function(o){return function(i){var c=u(i);return{sampleOnRight:O0(i)(n),sampleOnLeft:jw(i)(n),keepLatest:r(i),fix:Vw(i)(n),once:Ww(i)(n),Plus0:function(){return t},Alt1:function(){return a},Filterable2:function(){return c}}}}},Qw=function(n){var e=tu(n)(n.Plus0()),r=Hn(n.Filterable2().Functor1());return function(t){var a=Df(e(t));return{mapWithIndex:function(u){return function(o){return a(function(i){return function(c){return new q(i+1|0,u(i)(c))}})(0)(o)}},Functor0:function(){return r}}}},ka=function(n){var e=ne(n),r=Ae(n.Functor0());return function(t){return function(a){return function(u){return e(u)(t(a(r(u)(pi))))}}}},Yw=function(n){return function(){var r=J([])(),t=J(!1)(),a=J(m0(w))();return Xn(function(u){return Ga(function(o){return function(i){return function(){var l=V(t)();x0(!l)(function(){var m=o(_0(n)(y0(u)))(function(){var v=Kn(ie)(r),f=Kn(ag);return function(d){return Gu(Dw(v(f(d))))}}())();return Gu(Yn(!0)(t))(),Gu(Yn(m)(a))()})();var s=o(u)(function(p){return function(){h0(V(a))();var v=V(r)();return Rw(v)(function(f){return i(p(f))})()}})();return s}}})})}},Kw=function(){var e=_a(),r=ho(b0(e.event))();return{poll:r.poll,push:e.push}},Xr=function(n){var e=ne(n),r=n.Functor0(),t=k(r),a=Ae(r),u=Hn(r);return{apply:function(o){return function(i){return function(c){return e(t(function(l){return function(s){return s.value0(l(s.value1))}})(o(a(c)(pi))))(i(t(q.create)(c)))}}},Functor0:function(){return u}}},jn=function(n){var e=k(n.Functor0()),r=Xr(n);return{pure:function(t){return function(a){return e(oo(t))(a)}},Apply0:function(){return r}}},tt=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Xu=function(){function n(){}return n.value=new n,n}(),Ci=function(){function n(){}return n.value=new n,n}(),Wf=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),T0=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),P0=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),H=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),A0={eq:function(n){return function(e){return n instanceof tt&&e instanceof tt?n.value0===e.value0:n instanceof Xu&&e instanceof Xu}}},qe=function(n){return new P0(n)},E0=function(n){return new T0(n)};const Xw=n=>e=>e[n];var Jw=function(n){return n},Zw=Hp(),I0=Jw,nb=function(n){return n},eb=function(n){return I0(ug(n))},rb=function(n){return vf(n)>0?new $(I0(n)):_.value},q0=function(n){return function(e){return n(nb(e))}},tb=q0(vf),ab=function(){return q0(Zw)},F0={proof:function(n){return n},Coercible0:function(){}},ub=function(n){return n.proof},Ju=function(n){return n.reflectType},ob=function(n){return Ju(n)},C0=Ce,B0=function(){return function(n){return n}},ib=function(n){return[n]},cb=function(n){var e=ob(n);return function(){return function(){return function(){return function(r){return Xw(e(r))}}}}},es=[],fb=function(){return function(){return function(n){return function(e){return ki(n)(e)}}}};function lb(n){return function(){var e={};for(var r in n)hasOwnProperty.call(n,r)&&(e[r]=n[r]);return e}}const Uf={};function sb(n){return n()}function vb(n,e){var r={};for(var t in n)hasOwnProperty.call(n,t)&&(r[t]=e(n[t]));return r}function pb(n,e){var r={};for(var t in n)hasOwnProperty.call(n,t)&&(r[t]=e(t)(n[t]));return r}function db(n){return function(e){return function(r){return function(t){var a=r;function u(i){return function(c){return e(c)(i)(t[i])}}for(var o in t)hasOwnProperty.call(t,o)&&(a=n(a)(u(o)));return a}}}}function D0(n){return function(e){var r=[];for(var t in e)hasOwnProperty.call(e,t)&&r.push(n(t)(e[t]));return r}}function mb(n){return function(e){return function(r){return function(){return r[n]=e,r}}}}const hb=function(n){return function(e){return function(){return delete e[n],e}}};var R0=Pr(he),xb=An(Tn),N0=D0(function(n){return function(e){return e}}),yb=lb,z0=function(n){return function(e){return sb(function(){var t=yb(e)();return n(t)(),t})}},gb=function(n){return function(e){return pb(e,n)}},Zu=function(n){return function(e){return z0(mb(n)(e))}},L0={map:function(n){return function(e){return vb(e,n)}}},wb={mapWithIndex:gb,Functor0:function(){return L0}},bb=function(){return qr},Bi=db(oo),M0=function(n){var e=se(n.Semigroup0()),r=Xe(n);return function(t){return Bi(function(a){return function(u){return function(o){return e(a)(t(u)(o))}}})(r)}},Hf={foldl:function(n){return Bi(function(e){return function(r){return n(e)}})},foldr:function(n){return function(e){return function(r){return R0(n)(e)(N0(r))}}},foldMap:function(n){var e=M0(n);return function(r){return e(Fn(r))}}},$b={foldlWithIndex:function(n){return Bi(Kn(n))},foldrWithIndex:function(n){return function(e){return function(r){return R0(jy(n))(e)(D0(q.create)(r))}}},foldMapWithIndex:function(n){return M0(n)},Foldable0:function(){return Hf}},_b={traverseWithIndex:function(n){var e=n.Apply0(),r=ne(e),t=k(e.Functor0()),a=S(n);return function(u){return function(o){return Bi(function(i){return function(c){return function(l){return r(t(Kn(Zu(c)))(i))(u(c)(l))}}})(a(Uf))(o)}}},FunctorWithIndex0:function(){return wb},FoldableWithIndex1:function(){return $b},Traversable2:function(){return jf}},jf={traverse:function(n){var e=fg(_b)(n);return function(r){return e(Fn(r))}},sequence:function(n){return po(jf)(n)(xb)},Functor0:function(){return L0},Foldable1:function(){return Hf}},rs=function(n){return z0(hb(n))};function Ob(n){var e={};for(var r in n)({}).hasOwnProperty.call(n,r)&&(e[r]=n[r]);return e}function kb(n){return function(e){return function(r){return r[n]=e,r}}}function Sb(n){return function(e){return function(r){return r[n]=e(r[n]),r}}}function Tb(n,e){var r={};for(var t in e)({}).hasOwnProperty.call(e,t)&&(r[t]=e[t]);for(var a in n)({}).hasOwnProperty.call(n,a)&&(r[a]=n[a]);return r}var W0=gi,Pb=function(){return function(){return function(n){var e=da(n);return function(r){return function(t){return function(a){return Sb(e(r))(t)(a)}}}}}},U0=function(){return function(){return function(n){var e=da(n);return function(r){return function(t){return function(a){return kb(e(r))(t)(a)}}}}}},Ab=Tn,H0=function(n){return function(e){return n(Ob(e))}},Eb=Kn(H0)({}),Ib=k(Ce),j0=je(),no=Be(j0),qb=Zn(vo),La=S(Le),Pn=Ie(Ti),ts=Oa(j0)(U)(U),Ye=Pe(rr),Qu=Jn(_i),ei=k(rr),Pa=xa(ha),as=se(pp(vx)),us=S(Zh),os=se(Vp),Fb=se(ma),Cb=Ae(U),is=zt(ye),Vf=An(Tn),V0=vr(Le),di=V0(he),Bb=V0(tr),Gf=Dt(vo),Db=Pt(Hf),Rb=Ae(Ce),Ao=B0(),Nb=Ir(xe),zb=k(C0),Lb=n0(Rg),Mb=Yr(ya),Wb=ip(W0),Ub=U0()()({reflectSymbol:function(){return"id"}}),Hb=Pb()()({reflectSymbol:function(){return"parent"}}),jb=Ya(Le),Vb=Oi(Ef),Gb=gx(xx),Qb=yr(lo),Eo=function(){function n(){}return n.value=new n,n}(),Yb=function(){function n(){}return n.value=new n,n}(),Lr=function(n){return function(e){return function(r){return function(t){var a=function(u){return u(r)(t)};if(e instanceof P0)return Xn(function(u){return Rf(Ib(function(o){return no(Lr(n)(o)(r)(t))(u)})(e.value0))});if(e instanceof H)return a(n.toElt(e.value0));if(e instanceof T0)return Xn(function(u){return Ga(function(o){return function(i){return function(){var l=J(La(w))(),s=Pn(J(Uf))(),p=o(u)(function(m){return function(){var f=Pn(n.ids(t))();i(m(n.deferPayload(t)(r.deferralPath)(n.forcePayload(t)(kg(r.deferralPath)(f)))))();var d=s0(),h=o(ts(e.value0)(u))(d.push)(),b={unsubscribe:h,event:d.event};Ye(ie(function(g){return Qu(g)(b.unsubscribe)})(l))();var O=o(b.event)(function(g){return function(){var R=Pn(n.ids(t))(),Un=Pn(n.ids(t))(),Nn=Pn(J(La(w)))(),B=Pn(n.ids(t))(),A=Pn(J(La(w)))(),yn=Pn(J([]))(),Sn=Pn(ei(tt.create)(function(){if(r.scope instanceof Xu)return ei(Pa)(n.ids(t));if(r.scope instanceof tt)return ei(as(us(r.scope.value0))(as(us("!"))(Pa)))(n.ids(t));throw new Error("Failed pattern match at Bolson.Control (line 733, column 15 - line 735, column 74): "+[r.scope.constructor.name])}()))(),mn=Pn(J(Eo.value))();Ye(Pn(Yn(Eo.value)(mn)))();var bn=no(Lr(n)(Xa(g))(function(){var Y={};for(var Gn in r)({}).hasOwnProperty.call(r,Gn)&&(Y[Gn]=r[Gn]);return Y.scope=Sn,Y.deferralPath=os(r.deferralPath)(new E(f,new E(R,wn.value))),Y.raiseId=function(Re){return Ye(ie(Fb([Re]))(yn))},Y}())(t))(Cb(is(b.event))(Vf)),Me=Pn(o(bn)(function(Y){return i(m(Y))}))();Ye(Pn(ie(Zu(Pa(B))(Me))(s)))(),Ye(Pn(Yn(Me)(A)))();var We=Pn(o(ts(Yu(g))(is(b.event)))(function(Y){return function(){var Re=Pn(V(mn))();if(Y instanceof Wf&&Re instanceof Eo){var _t=Pn(V(yn))();return di(_t)(function(){var un=n.doLogic(Y.value0)(t);return function(on){return i(m(un(on)))}}())()}if(Y instanceof Ci&&Re instanceof Eo){Ye(Pn(Yn(Yb.value)(mn)))();var Ta=Pn(V(yn))();di(Ta)(function(un){return Bb(r.parent)(function(on){return i(m(n.disconnectElement(t)({id:un,parent:on,scope:Sn})))})})(),i(m(n.forcePayload(t)(os(r.deferralPath)(new E(f,new E(R,wn.value))))))();var P=Pn(V(Nn))();Pn(P)();var fn=Pn(V(A))();return Pn(fn)(),Ye(Pn(ie(rs(Pa(Un)))(s)))(),Ye(Pn(ie(rs(Pa(B)))(s)))()}return w}}))();return Ye(Pn(Yn(We)(Nn)))(),Ye(Pn(ie(Zu(Pa(Un))(We))(s)))()}})();return Ye(ie(function(g){return Qu(g)(O)})(l))()}})();return function(){return Pn(Gf(V(l)))(),p(),qb(V(s))(Db(Qu)(La(w)))()}}}})});throw new Error("Failed pattern match at Bolson.Control (line 702, column 17 - line 800, column 58): "+[e.constructor.name])}}}},Kb=function(n){return function(e){return function(r){var t=function(a){return function(u){return Xn(function(o){return Ga(function(i){return function(c){return function(){var s=J(La(w))(),p=i(o)(function(m){return function(){var f=J(_.value)(),d=r(new H(e.fromElt(function(b){return function(O){return Xn(function(g){return Ga(function(G){return function(R){return G(g)(function(Un){return function(){var B=V(f)();return B instanceof $&&b.parent instanceof $&&B.value0!==b.parent.value0?R(Un(e.connectToParent(u)({id:B.value0,parent:b.parent.value0})))():w}})}})})}}))),h=i(no(Lr(n)(d)(function(){var b={};for(var O in a)({}).hasOwnProperty.call(a,O)&&(b[O]=a[O]);return b.parent=a.parent,b.scope=a.scope,b.raiseId=function(g){return function(){return a.raiseId(g)(),Ye(Yn(new $(g))(f))()}},b}())(u))(o))(c)();return Ye(ie(function(b){return Qu(b)(h)})(s))()}})();return function(){return Gf(V(s))(),p()}}}})})}};return new H(e.fromElt(t))}}},Qf=function(n){return Xn(function(e){return Ga(function(r){return function(t){return function(){var u=J(La(w))(),o=r(e)(function(i){return n(i)(e)(function(c){return t(i(c))})(function(c){return function(l){return function(){var p=r(c)(l(t))();return Ye(ie(function(m){return Qu(m)(p)})(u))()}}})})();return function(){return Pn(Gf(V(u)))(),o()}}}})})},Xb=function(n){return Qf(function(e){return n})},cn=function(n){return Xb(function(e){return function(r){return function(t){return n(e)(r)(Kn(t)(Vf))}}})},Jb=function(){return function(){return function(){return function(n){return function(e){return function(r){return function(t){return function(a){return function(u){var o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=Mg(Rb(Ao(a))({id:"",entity:new H(t.fromEltO1(function(b){return function(O){return Xn(function(g){return Nb})}}))}))(),f=J(0)(),d=function(){var O=ei(qr)(Ug(v))(),g=zb(function(R){return function(Un){return new H(t.fromEltO1(function(Nn){return function(B){return cn(function(A){return function(yn){return function(Sn){return function(){return Pn(Nn.raiseId(R.id))(),di(Lb([Mb(Nn.parent)(function(bn){return t.giveNewParent(B)(H0(Wb(Ub(x.value)(R.id))(Hb(x.value)(Fn(bn))))(Nn))(R.entity)(Un)})]))(yn)()}}}})}}))}})(O),G=Lr(r)(u(g))(i)(c);return p(no(G)(l))(),jb(!n)(di(Ao(O))(function(R){return s(r.deferPayload(c)(i.deferralPath)(t.deleteFromCache(c)({id:R.id})))}))()},h=Vb(function(b){return Gb(function(O){return function(g){return g instanceof H?function(G){return no(G(function(){var R={};for(var Un in i)({}).hasOwnProperty.call(i,Un)&&(R[Un]=i[Un]);return R.parent=_.value,R.scope=e(i.scope),R.raiseId=function(Nn){return function(){Wg(b)({id:Nn,entity:g})(v)();var A=V(f)(),yn=(A+1|0)===vf(Ao(a));return yn?d():Ye(ie(Qb(1))(f))()}},R}())(c))(l)}(t.toElt(g.value0)):O(t.wrapElt(g))}})})(Ao(a));return p(Rf(h))()}}}})}};return new H(t.fromEltO2(o))}}}}}}}}},Zb=Jb()()(),n$=function(){return function(){return function(){return function(n){return function(e){return function(r){return function(t){return Zb(!1)(Vf)(n)(e)(r)(t)}}}}}}},e$=An(Tn),au={dimap:function(n){return function(e){return function(r){return function(t){return e(r(n(t)))}}}}},Yf=function(n){return n.dimap},Di=function(n){var e=Yf(n);return function(r){return e(r)(e$)}},r$=function(n){return n},G0=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Q0=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),t$=function(){function n(){}return n.value=new n,n}(),a$=function(n){return n},Y0=wa(),ar=a$,Fr=function(){return G0.create}(),Kf=function(){return Q0.create}(),Xf=function(){var n=k(io)(k(Ee)(Fn(!0)));return function(e){return r$(n(e))}}(),u$=Di(au),o$=hf(A0),ia=xa(ha),i$=S(Le),c$=Be(je()),cs=vr(Le)(he),f$=se(ma),fs=k(Hn(U)),ls=k(Ce),l$=Ir(xe),s$=function(n){return n},v$=function(n){return function(e){var r=function(t){var a=function(u){return u instanceof H?new H(u$(function(o){return{pos:n,deferralPath:o.deferralPath,dynFamily:o.dynFamily,ez:o.ez,parent:o.parent,raiseId:o.raiseId,scope:o.scope}})(u.value0)):u};return a(t)};return r(e)}},K0=function(n){return v$($.create(n))},p$=function(n){return new Wf(n)},d$=function(){return Ci.value}(),Ri={doLogic:function(n){return function(e){return function(r){return e.sendToPos({id:r,pos:n})}}},deferPayload:function(n){return n.deferPayload},forcePayload:function(n){return n.forcePayload},ids:function(){var n=ba();return function(e){return function(r){return r.ids}(n(e))}}(),disconnectElement:function(n){return function(e){return n.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:o$})}},toElt:function(n){return n}},m$=function(n){return function(e){return function(r){return Lr(Ri)(function(t){return t}(n))(e)(r)}}},X0=function(n){return function(e){var r=function(a){return function(u){return function(o){return cn(function(i){return function(c){return function(l){return function(){var p=o.ids();u.raiseId(ia(p))();var m=function(){if(u.parent instanceof _){var f=o.ids();return new q([o.makeElement({id:ia(f),parent:_.value,scope:u.scope,tag:"div",ns:_.value,pos:_.value,dynFamily:_.value})],ia(f))}if(u.parent instanceof $)return new q([],u.parent.value0);throw new Error("Failed pattern match at Deku.Core (line 355, column 32 - line 371, column 31): "+[u.parent.constructor.name])}(),v=c$(m$(a)({parent:new $(m.value1),scope:u.scope,ez:!1,raiseId:function(f){return i$(w)},deferralPath:u.deferralPath,pos:_.value,dynFamily:new $(ia(p))})(o))(i);return cs(f$(m.value0)([o.makeDynBeacon({id:ia(p),parent:new $(m.value1),scope:u.scope,dynFamily:u.dynFamily,pos:u.pos}),o.attributeParent({id:ia(p),parent:m.value1,pos:u.pos,dynFamily:u.dynFamily,ez:u.ez})]))(c)(),cs([o.removeDynBeacon({id:ia(p)})])(function(){var f=o.deferPayload(u.deferralPath);return function(d){return c(f(d))}}())(),l(v)()}}}})}}},t=function(a){return new H(r(a))};return t(function(a){return a}(n(e)))}},h$=function(){var n=function(t){return E0(t)},e=function(t){return new q(fs(function(a){return a})(t.value0),function(a){return a}(t.value1))},r=function(t){return n(fs(e)(t))};return X0(r)}(),x$=function(){var n=function(r){return qe(ls(function(t){return t})(r))},e=function(r){return n(ls(function(t){return t})(r))};return X0(e)}(),y$={append:function(n){return function(e){return x$([n,e])}}},g$=function(){return{mempty:new H(function(n){return function(e){return Xn(function(r){return l$})}}),Semigroup0:function(){return y$}}}(),w$=function(n){var e=da(n);return function(){return function(){return function(r){return function(t){return function(a){return mf(e(r))(t)(a)}}}}}},b$=function(){return function(){return function(n){return function(e){return Tb(n,e)}}}},Ni=function(n){var e=da(n);return function(){return function(r){return function(t){return df(e(r))(t)}}}},Tr=xa(ha),J0=vr(Le),Z0=J0(tr),nd=je(),ed=Kr(nd)(U)(U),$$=S(jn(Z)),_$=J0(he),lc=S(bg),rd=Be(nd),td=S(Le),O$=Ir(gr(xe)),k$=wa(),S$=Oi(Ef),ss=k(io),T$=function(n){return function(e){return function(r){return n.setText(function(t){return{id:e,text:t}}(r))}}},ad=function(n){return function(e){return function(r){if(r.value instanceof G0)return n.setProp({id:e,key:r.key,value:r.value.value0});if(r.value instanceof Q0)return n.setCb({id:e,key:r.key,value:r.value.value0});if(r.value instanceof t$)return n.unsetAttribute({id:e,key:r.key});throw new Error("Failed pattern match at Deku.Control (line 66, column 3 - line 69, column 41): "+[r.value.constructor.name])}}},wr=function(n){var e=function(t){return function(a){return cn(function(u){return function(o){return function(i){return function(){var l=a.ids();return t.raiseId(Tr(l))(),o(a.makeText({id:Tr(l),parent:t.parent,pos:t.pos,scope:t.scope,dynFamily:t.dynFamily}))(),Z0(t.parent)(function(s){return o(a.attributeParent({id:Tr(l),parent:s,pos:t.pos,dynFamily:t.dynFamily,ez:t.ez}))})(),o(a.deferPayload(t.deferralPath)(a.deleteFromCache({id:Tr(l)})))(),i(ed(function(s){return function(p){return p(T$(a)(Tr(l))(s))}})(n)(u))()}}}})}},r=new H(e);return r},Te=function(n){return wr($$(n))},ud=function(n){return function(e){return function(r){return Lr(Ri)(n)(e)(r)}}},P$=function(n){return function(e){var r=function(t){return function(a){return cn(function(u){return function(o){return function(i){return function(){var l=a.ids();return o(a.makeRoot({id:"deku-root",root:n}))(),_$([a.forcePayload(lc(l)),a.deleteFromCache({id:"deku-root"})])(function(){var s=a.deferPayload(lc(l));return function(p){return o(s(p))}}())(),i(rd(ud(t)({parent:new $("deku-root"),deferralPath:lc(l),scope:new tt("rootScope"),raiseId:function(s){return td(w)},ez:!0,pos:_.value,dynFamily:_.value})(a))(u))()}}}})}};return r(e)}},A$=function(n){return function(e){return function(r){return function(t){var a=function(u){return function(o){return cn(function(i){return function(c){return function(l){return function(){var p=o.ids();return u.raiseId(Tr(p))(),c(o.makeElement({id:Tr(p),parent:u.parent,scope:u.scope,ns:n,tag:e,pos:u.pos,dynFamily:u.dynFamily}))(),Z0(u.parent)(function(m){return c(o.attributeParent({id:Tr(p),parent:m,pos:u.pos,dynFamily:u.dynFamily,ez:u.ez}))})(),c(o.deferPayload(u.deferralPath)(o.deleteFromCache({id:Tr(p)})))(),l(rd(ud(t)({parent:new $(Tr(p)),deferralPath:u.deferralPath,scope:u.scope,ez:!0,raiseId:function(m){return td(w)},pos:_.value,dynFamily:_.value})(o))(i))(),l(ed(function(m){return function(v){return v(ad(o)(Tr(p))(Y0(m)))}})(r)(i))()}}}})}};return a}}}},ft=function(n){return function(e){return function(r){return function(t){var a=function(o){return o.length===0?O$:o.length===1?o[0]:$0(o)},u=function(o){return new H(A$(n)(e)(a(r))(qe(k$(o))))};return u(S$(ss(ss(function(o){return o}))(K0))(t))}}}},Jf=function(){return ft(_.value)("span")}(),E$=Jf([]),I$=function(){return ft(_.value)("pre")}(),q$=I$([]),xo=function(){return ft(_.value)("input")}(),X=function(){return ft(_.value)("div")}(),zn=X([]),F$=function(){return ft(_.value)("code")}(),C$=F$([]),B$=function(){return ft(_.value)("canvas")}(),Jr=function(){return ft(_.value)("button")}(),D$=function(){return ft(_.value)("audio")}(),R$=function(){return ft(_.value)("a")}(),N$=k(Hn(U)),z$=S(jn(Z)),od=N$(function(n){return ar(function(e){return{key:"@self@",value:e}}(Kf(Xf(n))))}),L$=function(n){return od(z$(n))},Ke=function(){var n={},e="Pure",r="Throw",t="Catch",a="Sync",u="Async",o="Bind",i="Bracket",c="Fork",l="Sequential",s="Map",p="Apply",m="Alt",v="Cons",f="Resume",d="Release",h="Finalizer",b="Finalized",O="Forked";function g(P,fn,un,on){this.tag=P,this._1=fn,this._2=un,this._3=on}function G(P){var fn=function(un,on,F){return new g(P,un,on,F)};return fn.tag=P,fn}function R(P){return new g(e,void 0)}function Un(P){try{P()}catch(fn){setTimeout(function(){throw fn},0)}}function Nn(P,fn,un){try{return fn(un())}catch(on){return P(on)}}function B(P,fn,un){try{return fn(un)()}catch(on){return un(P(on))(),R}}var A=function(){var P=1024,fn=0,un=0,on=new Array(P),F=!1;function T(){var $n;for(F=!0;fn!==0;)fn--,$n=on[un],on[un]=void 0,un=(un+1)%P,$n();F=!1}return{isDraining:function(){return F},enqueue:function($n){var K;fn===P&&(K=F,T(),F=K),on[(un+fn)%P]=$n,fn++,F||T()}}}();function yn(P){var fn={},un=0,on=0;return{register:function(F){var T=un++;F.onComplete({rethrow:!0,handler:function($n){return function(){on--,delete fn[T]}}})(),fn[T]=F,on++},isEmpty:function(){return on===0},killAll:function(F,T){return function(){if(on===0)return T();var $n=0,K={};function fe(En){K[En]=fn[En].kill(F,function(oe){return function(){delete K[En],$n--,P.isLeft(oe)&&P.fromLeft(oe)&&setTimeout(function(){throw P.fromLeft(oe)},0),$n===0&&T()}})()}for(var Fe in fn)fn.hasOwnProperty(Fe)&&($n++,fe(Fe));return fn={},un=0,on=0,function(En){return new g(a,function(){for(var oe in K)K.hasOwnProperty(oe)&&K[oe]()})}}}}}var Sn=0,mn=1,bn=2,Me=3,We=4,Y=5,Gn=6;function Re(P,fn,un){var on=0,F=Sn,T=un,$n=null,K=null,fe=null,Fe=null,En=null,oe=0,oa=0,pr=null,Ot=!0;function kt(D){for(var N,ln,hn;;)switch(N=null,ln=null,hn=null,F){case bn:F=mn;try{T=fe(T),Fe===null?fe=null:(fe=Fe._1,Fe=Fe._2)}catch(Ne){F=Y,$n=P.left(Ne),T=null}break;case Me:P.isLeft(T)?(F=Y,$n=T,T=null):fe===null?F=Y:(F=bn,T=P.fromRight(T));break;case mn:switch(T.tag){case o:fe&&(Fe=new g(v,fe,Fe)),fe=T._2,F=mn,T=T._1;break;case e:fe===null?(F=Y,T=P.right(T._1)):(F=bn,T=T._1);break;case a:F=Me,T=Nn(P.left,P.right,T._1);break;case u:F=We,T=B(P.left,T._1,function(Ne){return function(){on===D&&(on++,A.enqueue(function(){on===D+1&&(F=Me,T=Ne,kt(on))}))}});return;case r:F=Y,$n=P.left(T._1),T=null;break;case t:fe===null?En=new g(v,T,En,K):En=new g(v,T,new g(v,new g(f,fe,Fe),En,K),K),fe=null,Fe=null,F=mn,T=T._1;break;case i:oe++,fe===null?En=new g(v,T,En,K):En=new g(v,T,new g(v,new g(f,fe,Fe),En,K),K),fe=null,Fe=null,F=mn,T=T._1;break;case c:F=Me,N=Re(P,fn,T._2),fn&&fn.register(N),T._1&&N.run(),T=P.right(N);break;case l:F=mn,T=Ta(P,fn,T._1);break}break;case Y:if(fe=null,Fe=null,En===null)F=Gn,T=K||$n||T;else switch(N=En._3,hn=En._1,En=En._2,hn.tag){case t:K&&K!==N&&oe===0?F=Y:$n&&(F=mn,T=hn._2(P.fromLeft($n)),$n=null);break;case f:K&&K!==N&&oe===0||$n?F=Y:(fe=hn._1,Fe=hn._2,F=bn,T=P.fromRight(T));break;case i:oe--,$n===null&&(ln=P.fromRight(T),En=new g(v,new g(d,hn._2,ln),En,N),(K===N||oe>0)&&(F=mn,T=hn._3(ln)));break;case d:En=new g(v,new g(b,T,$n),En,K),F=mn,K&&K!==N&&oe===0?T=hn._1.killed(P.fromLeft(K))(hn._2):$n?T=hn._1.failed(P.fromLeft($n))(hn._2):T=hn._1.completed(P.fromRight(T))(hn._2),$n=null,oe++;break;case h:oe++,En=new g(v,new g(b,T,$n),En,K),F=mn,T=hn._1;break;case b:oe--,F=Y,T=hn._1,$n=hn._2;break}break;case Gn:for(var te in pr)pr.hasOwnProperty(te)&&(Ot=Ot&&pr[te].rethrow,Un(pr[te].handler(T)));pr=null,K&&$n?setTimeout(function(){throw P.fromLeft($n)},0):P.isLeft(T)&&Ot&&setTimeout(function(){if(Ot)throw P.fromLeft(T)},0);return;case Sn:F=mn;break;case We:return}}function re(D){return function(){if(F===Gn)return Ot=Ot&&D.rethrow,D.handler(T)(),function(){};var N=oa++;return pr=pr||{},pr[N]=D,function(){pr!==null&&delete pr[N]}}}function L(D,N){return function(){if(F===Gn)return N(P.right(void 0))(),function(){};var ln=re({rethrow:!1,handler:function(){return N(P.right(void 0))}})();switch(F){case Sn:K=P.left(D),F=Gn,T=K,kt(on);break;case We:K===null&&(K=P.left(D)),oe===0&&(F===We&&(En=new g(v,new g(h,T(D)),En,K)),F=Y,T=null,$n=null,kt(++on));break;default:K===null&&(K=P.left(D)),oe===0&&(F=Y,T=null,$n=null)}return ln}}function tn(D){return function(){var N=re({rethrow:!1,handler:D})();return F===Sn&&kt(on),N}}return{kill:L,join:tn,onComplete:re,isSuspended:function(){return F===Sn},run:function(){F===Sn&&(A.isDraining()?kt(on):A.enqueue(function(){kt(on)}))}}}function _t(P,fn,un,on){var F=0,T={},$n=0,K={},fe=new Error("[ParAff] Early exit"),Fe=null,En=n;function oe(re,L,tn){var D=L,N=null,ln=null,hn=0,te={},Ne,xu;n:for(;;)switch(Ne=null,D.tag){case O:if(D._3===n&&(Ne=T[D._1],te[hn++]=Ne.kill(re,function(Vh){return function(){hn--,hn===0&&tn(Vh)()}})),N===null)break n;D=N._2,ln===null?N=null:(N=ln._1,ln=ln._2);break;case s:D=D._2;break;case p:case m:N&&(ln=new g(v,N,ln)),N=D,D=D._1;break}if(hn===0)tn(P.right(void 0))();else for(xu=0,Ne=hn;xu<Ne;xu++)te[xu]=te[xu]();return te}function oa(re,L,tn){var D,N,ln,hn,te,Ne;for(P.isLeft(re)?(D=re,N=null):(N=re,D=null);;){if(ln=null,hn=null,te=null,Ne=null,Fe!==null)return;if(L===null){on(D||N)();return}if(L._3!==n)return;switch(L.tag){case s:D===null?(L._3=P.right(L._1(P.fromRight(N))),N=L._3):L._3=D;break;case p:if(ln=L._1._3,hn=L._2._3,D){if(L._3=D,te=!0,Ne=$n++,K[Ne]=oe(fe,D===ln?L._2:L._1,function(){return function(){delete K[Ne],te?te=!1:tn===null?oa(D,null,null):oa(D,tn._1,tn._2)}}),te){te=!1;return}}else{if(ln===n||hn===n)return;N=P.right(P.fromRight(ln)(P.fromRight(hn))),L._3=N}break;case m:if(ln=L._1._3,hn=L._2._3,ln===n&&P.isLeft(hn)||hn===n&&P.isLeft(ln))return;if(ln!==n&&P.isLeft(ln)&&hn!==n&&P.isLeft(hn))D=N===ln?hn:ln,N=null,L._3=D;else if(L._3=N,te=!0,Ne=$n++,K[Ne]=oe(fe,N===ln?L._2:L._1,function(){return function(){delete K[Ne],te?te=!1:tn===null?oa(N,null,null):oa(N,tn._1,tn._2)}}),te){te=!1;return}break}tn===null?L=null:(L=tn._1,tn=tn._2)}}function pr(re){return function(L){return function(){delete T[re._1],re._3=L,oa(L,re._2._1,re._2._2)}}}function Ot(){var re=mn,L=un,tn=null,D=null,N,ln;n:for(;;)switch(N=null,ln=null,re){case mn:switch(L.tag){case s:tn&&(D=new g(v,tn,D)),tn=new g(s,L._1,n,n),L=L._2;break;case p:tn&&(D=new g(v,tn,D)),tn=new g(p,n,L._2,n),L=L._1;break;case m:tn&&(D=new g(v,tn,D)),tn=new g(m,n,L._2,n),L=L._1;break;default:ln=F++,re=Y,N=L,L=new g(O,ln,new g(v,tn,D),n),N=Re(P,fn,N),N.onComplete({rethrow:!1,handler:pr(L)})(),T[ln]=N,fn&&fn.register(N)}break;case Y:if(tn===null)break n;tn._1===n?(tn._1=L,re=mn,L=tn._2,tn._2=n):(tn._2=L,L=tn,D===null?tn=null:(tn=D._1,D=D._2))}for(En=L,ln=0;ln<F;ln++)T[ln].run()}function kt(re,L){Fe=P.left(re);var tn;for(var D in K)if(K.hasOwnProperty(D)){tn=K[D];for(D in tn)tn.hasOwnProperty(D)&&tn[D]()}K=null;var N=oe(re,En,L);return function(ln){return new g(u,function(hn){return function(){for(var te in N)N.hasOwnProperty(te)&&N[te]();return R}})}}return Ot(),function(re){return new g(u,function(L){return function(){return kt(re,L)}})}}function Ta(P,fn,un){return new g(u,function(on){return function(){return _t(P,fn,un,on)}})}return g.EMPTY=n,g.Pure=G(e),g.Throw=G(r),g.Catch=G(t),g.Sync=G(a),g.Async=G(u),g.Bind=G(o),g.Bracket=G(i),g.Fork=G(c),g.Seq=G(l),g.ParMap=G(s),g.ParApply=G(p),g.ParAlt=G(m),g.Fiber=Re,g.Supervisor=yn,g.Scheduler=A,g.nonCanceler=R,g}();const M$=Ke.Pure;function W$(n){return function(e){return e.tag===Ke.Pure.tag?Ke.Pure(n(e._1)):Ke.Bind(e,function(r){return Ke.Pure(n(r))})}}function U$(n){return function(e){return Ke.Bind(n,e)}}const H$=Ke.Sync;function j$(n){return function(e){return Ke.ParMap(n,e)}}function V$(n){return function(e){return Ke.ParApply(n,e)}}function G$(n){return function(e){return Ke.ParAlt(n,e)}}const Zf=Ke.Async;function Q$(n,e){return function(){return Ke.Fiber(n,null,e)}}const Y$=function(){function n(r,t){return r===0&&typeof setImmediate<"u"?setImmediate(t):setTimeout(t,r)}function e(r,t){return r===0&&typeof clearImmediate<"u"?clearImmediate(t):clearTimeout(t)}return function(r,t){return Ke.Async(function(a){return function(){var u=n(t,a(r()));return function(){return Ke.Sync(function(){return r(e(t,u))})}}})}}(),K$=Ke.Seq;function mi(n){return new Error(n)}function id(n){return function(){throw n}}function X$(n){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?n(r)():n(new Error(r.toString()))()}}}}var J$=function(n){return n.throwError},Z$={throwError:id,Monad0:function(){return so}},n1={catchError:Kn(X$),MonadThrow0:function(){return Z$}},e1=function(n){return n.catchError},r1=function(n){var e=e1(n),r=n.MonadThrow0().Monad0(),t=k(r.Bind1().Apply0().Functor0()),a=S(r.Applicative0());return function(u){return e(t(Ln.create)(u))(function(o){return a(Qn.create(o))})}},t1=function(n){return n.state},ge={liftEffect:An(Tn),Monad0:function(){return so}},br=function(n){return n.liftEffect},a1=k(Jx),cd=function(n){return n},u1=function(n){return n},o1=function(n){return function(e){return n(e)}},nl=function(n){var e=k(n);return{map:function(r){return o1(e(a1(r)))}}},fd=function(n){return{Applicative0:function(){return sd(n)},Bind1:function(){return i1(n)}}},i1=function(n){var e=Zn(n.Bind1()),r=S(n.Applicative0());return{bind:function(t){return function(a){return e(t)(Ha(function(u){return r(Qn.create(u))})(function(u){var o=a(u);return o}))}},Apply0:function(){return ld(n)}}},ld=function(n){var e=nl(n.Bind1().Apply0().Functor0());return{apply:fo(fd(n)),Functor0:function(){return e}}},sd=function(n){return{pure:function(){var e=S(n.Applicative0());return function(r){return cd(e(Ln.create(r)))}}(),Apply0:function(){return ld(n)}}},c1=function(n){var e=fd(n);return{throwError:function(){var r=S(n.Applicative0());return function(t){return cd(r(Qn.create(t)))}}(),Monad0:function(){return e}}},f1=function(n){var e=se(n);return function(r){var t=r.Bind1(),a=Zn(t),u=S(r.Applicative0()),o=nl(t.Apply0().Functor0());return{alt:function(i){return function(c){return a(i)(function(l){if(l instanceof Ln)return u(new Ln(l.value0));if(l instanceof Qn)return a(c)(function(s){if(s instanceof Ln)return u(new Ln(s.value0));if(s instanceof Qn)return u(new Qn(e(l.value0)(s.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[s.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[l.constructor.name])})}},Functor0:function(){return o}}}},yo=function(n){return n.sequential},uu=function(n){return n.parallel},l1=An(Tn),s1=function(n){var e=yo(n),r=it(n.Applicative1()),t=uu(n);return function(a){var u=r(a);return function(o){var i=u(function(c){return t(o(c))});return function(c){return e(i(c))}}}},v1=function(n){var e=yo(n),r=n.Applicative1(),t=uu(n);return function(a){var u=po(a)(r);return function(o){var i=u(function(c){return t(o(c))});return function(c){return e(i(c))}}}},p1=function(n){var e=s1(n);return function(r){return e(r)(l1)}},vs=Gy()(),d1=function(n){return n},m1=function(n){return n},h1=function(n){return n.toDuration},x1={fromDuration:vs(d1)(function(n){return n*1e3}),toDuration:vs(m1)(function(n){return n/1e3})};const y1=function(n){return n()};var vd=function(n,e,r){var t=0,a;return function(u){if(t===2)return a;if(t===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return t=1,a=r(),t=2,a}},g1=S(gn),pd=Pe(Ee),dd=k(Ee),w1=function(n){return n},zi={map:j$},go={map:W$},b1=function(){var n=function(t){if(t instanceof Ln)return t.value0;if(t instanceof Qn)return Wa("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): "+[t.constructor.name])},e=function(t){if(t instanceof Qn)return t.value0;if(t instanceof Ln)return Wa("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): "+[t.constructor.name])},r=function(t){if(t instanceof Qn)return!0;if(t instanceof Ln)return!1;throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): "+[t.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:n,left:Qn.create,right:Ln.create}}(),$1=function(n){return Q$(b1,n)},ou=function(n){return function(){var r=$1(n)();return r.run(),r}},iu=function(n){return pd(ou(n))},_1=function(n){return Y$(Ln.create,n)},el={apply:V$,Functor0:function(){return zi}},rl={Applicative0:function(){return lt},Bind1:function(){return Zr}},Zr={bind:U$,Apply0:function(){return tl(0)}},lt={pure:M$,Apply0:function(){return tl(0)}},tl=vd("applyAff","Effect.Aff",function(){return{apply:fo(rl),Functor0:function(){return go}}}),O1=tl(73),md=S(lt),k1=Zn(Zr),Ge={liftEffect:H$,Monad0:function(){return rl}},jc=br(Ge),hd=function(n){return w1(Fn(jc(n)))},S1=function(n){return Zf(function(e){return dd(hd)(n.join(e))})},T1=function(n){return function(e){return k1(jc(e.isSuspended))(function(r){return r?jc(pd(e.kill(n,Fn(g1(w))))):Zf(function(t){return dd(hd)(e.kill(n,t))})})}},st={parallel:qr,sequential:K$,Monad0:function(){return rl},Applicative1:function(){return P1(0)}},P1=vd("applicativeParAff","Effect.Aff",function(){return{pure:function(){var n=uu(st);return function(e){return n(md(e))}}(),Apply0:function(){return el}}}),A1=p1(st)(he),E1={append:function(n){return function(e){return function(r){return A1([n(r),e(r)])}}}},I1=Fn(md(w)),q1={mempty:I1,Semigroup0:function(){return E1}},F1={alt:G$,Functor0:function(){return zi}};function C1(n){return n.target}var al=function(n){return p0(C1(n))},B1=k(Hn(U)),D1=ff(sf)(Zr),R1=br(Ge),me=function(n){var e=B1(Fn);return function(r){return n(e(r))}},N1=function(n){return L$(function(e){return iu(D1(_1(0))(function(){return R1(n(e))}))})},Cr=k(Hn(U)),vt=S(jn(Z)),z1=Cr(function(n){return ar(function(e){return{key:"type",value:e}}(Fr(n)))}),xd=function(n){return z1(vt(n))},Li=xd("range"),L1=Cr(function(n){return ar(function(e){return{key:"width",value:e}}(Fr(n)))}),M1=function(n){return L1(vt(n))},W1=Cr(function(n){return ar(function(e){return{key:"value",value:e}}(Fr(n)))}),Mi=function(n){return W1(vt(n))},eo=Cr(function(n){return ar(function(e){return{key:"style",value:e}}(Fr(n)))}),M=function(n){return eo(vt(n))},U1=Cr(function(n){return ar(function(e){return{key:"step",value:e}}(Fr(n)))}),Wi=function(n){return U1(vt(n))},H1=Cr(function(n){return ar(function(e){return{key:"src",value:e}}(Fr(n)))}),j1=Cr(function(n){return ar(function(e){return{key:"min",value:e}}(Fr(n)))}),Ui=function(n){return j1(vt(n))},V1=Cr(function(n){return ar(function(e){return{key:"max",value:e}}(Fr(n)))}),Hi=function(n){return V1(vt(n))},G1=Cr(function(n){return ar(function(e){return{key:"height",value:e}}(Fr(n)))}),Q1=function(n){return G1(vt(n))},Y1=Cr(function(n){return ar(function(e){return{key:"controls",value:e}}(Fr(n)))}),K1=function(n){return Y1(vt(n))},X1=Cr(function(n){return ar(function(e){return{key:"checked",value:e}}(Fr(n)))}),yd=k(Hn(U)),gd=S(jn(Z)),J1=yd(function(n){return ar(function(e){return{key:"input",value:e}}(Kf(Xf(n))))}),ji=function(n){return J1(gd(n))},le=yd(function(n){return ar(function(e){return{key:"click",value:e}}(Kf(Xf(n))))}),wo=function(n){return le(gd(n))},vn=function(n){return function(e){return n(e)}},Z1=hf(A0),ps=Ir(gr(xe)),Vi=je(),Gi=Be(Vi),wd=An(Tn),n2=Uw(Ka),bd=Hn(U),e2=Ae(bd),$d=k(bd),r2=Oi(Qw(ye)(Vi)),t2=Df(tu(ye)(xe)(Vi)),_d=ve(Ve(He)),a2=Ct(ru(U)(eu)(Vi)),u2=S(jn(Z)),o2={doLogic:function(n){return function(e){return function(r){return e.sendToPos({id:r,pos:n})}}},ids:function(){var n=ba();return function(e){return function(r){return r.ids}(n(e))}}(),deferPayload:function(n){return n.deferPayload},forcePayload:function(n){return n.forcePayload},disconnectElement:function(n){return function(e){return n.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:Z1})}},toElt:function(n){return n}},i2={sendTo:function(n){return ps},remove:function(n){return ps}},Qi=function(n){return function(e){return function(r){return Lr(o2)(n)(e)(r)}}},c2=function(n){return function(e){var r=function(a){return function(u){return Qf(function(o){return function(i){return function(c){return function(l){return function(){var p=Yw(n)(),m=e(p);return l(Gi(Qi(m)(a)(u))(i))(wd)()}}}}})}},t=new H(r);return t}},f2=function(n){return function(e){return function(r){var t=function(u){return function(o){return cn(function(i){return function(c){return function(l){return function(){var p=n2(),m=function(f){return function(d){return new q($0([e2(e.remove(d.value1))(Ci.value),$d(function(h){return s$(Wf.create(h))})(e.sendTo(d.value1)),p.poll(f)]),K0(d.value0)(r({value:d.value1,remove:p.push({address:f,payload:d$}),sendTo:function(h){return p.push(function(b){return{address:f,payload:b}}(p$(h)))}})))}},v=h$(r2(m)(n));return l(Gi(Qi(v)(u)(o))(i))()}}}})}},a=new H(t);return a}}},l2=function(n){return f2($d(function(e){return new q(0,e)})(n))},s2=function(n){return function(e){var r=function(a){return function(u){return Qf(function(o){return function(i){return function(c){return function(l){return function(){var p=ho(n)(),m=e(p.poll);return l(Gi(Qi(m)(a)(u))(i))(wd)()}}}}})}},t=new H(r);return t}},v2=function(n){return function(e){var r=function(){var t=function(a){return function(u){return new q(a+1|0,new q(a,u))}};return t2(t)(0)}();return vn(s2(r(e)))(function(t){return vn(c2(r(e)))(function(a){return vn(l2(_d(t)(a))({sendTo:i2.sendTo,remove:function(u){return a2(function(o){var i=o.value0===(u.value0+1|0);return i?new $(w):_.value})(t)}}))(function(u){return n(Xa(u.value))})})})}},p2=function(n){return function(e){return v2(e)(n)}},qn=function(n){var e=function(t){return function(a){return cn(function(u){return function(o){return function(i){return function(){var l=Kw(),s=n(new q(l.push,l.poll));return i(Gi(Qi(s)(t)(a))(u))()}}}})}},r=new H(e);return r},cu=function(n){return function(e){return vn(qn)(function(r){return e(new q(r.value0,_d(u2(n))(r.value1)))})}};const d2=n=>e=>()=>{e.units[n.id].unsubscribe=n.unsubscribe},m2=n=>e=>r=>()=>{if(r.units[e.id]){const t=r.units[e.parent].main;r.units[e.id].main&&r.units[e.id].main.parentNode||r.units[e.id].startBeacon&&r.units[e.id].startBeacon.parentNode||(e.ez?(()=>(r.units[e.id].main?t.appendChild(r.units[e.id].main):(t.appendChild(r.units[e.id].startBeacon),t.appendChild(r.units[e.id].endBeacon)),!0))():n(e.pos)(u=>()=>n(e.dynFamily)(o=>()=>{for(var i=0,c=0,l;c<t.childNodes.length;){if(t.childNodes[c].nodeType===8&&t.childNodes[c].nodeValue==="%-%"+o){c+=1;break}c++}const s=m=>{const v=t.childNodes[m];r.units[e.id].startBeacon?(t.insertBefore(r.units[e.id].startBeacon,v),t.insertBefore(r.units[e.id].endBeacon,v)):t.insertBefore(r.units[e.id].main,v)};for(;c<t.childNodes.length;){var p;if((p=t.childNodes[c].$dekuId)&&n(r.units[p].dynFamily)(v=>()=>n(r.units[p].pos)(d=>()=>o===v&&u<=d?(s(c),!0):!1)())())return!0;if(i===u||t.childNodes[c].nodeType===8&&t.childNodes[c].nodeValue==="%-%"+o+"%-%")return s(c),!0;t.childNodes[c].nodeType===8&&t.childNodes[c].nodeValue.substring(0,3)==="%-%"&&!l&&(l=t.childNodes[c].nodeValue+"%-%"),l||i++,t.childNodes[c].nodeType===8&&t.childNodes[c].nodeValue===l&&(l=void 0,i++),c++}return!1})())())||(e.parent.indexOf("@!%")!==-1?n(e.dynFamily)(o=>()=>(r.units[e.id].main?r.units[o].endBeacon.parentNode.insertBefore(r.units[e.id].main,r.units[o].endBeacon):(r.units[o].endBeacon.parentNode.insertBefore(r.units[e.id].endBeacon,r.units[o].endBeacon),r.units[o].endBeacon.parentNode.insertBefore(r.units[e.id].startBeacon,r.units[e.id].endBeacon)),!0))()||(r.units[e.id].main?t.parentNode.replaceChild(r.units[e.id].main,t):(t.parentNode.replaceChild(r.units[e.id].endBeacon,t),r.units[e.id].endBeacon.parentNode.insertBefore(r.units[e.id].startBeacon,r.units[e.id].endBeacon))):n(e.dynFamily)(o=>()=>(r.units[e.id].startBeacon?(t.insertBefore(r.units[e.id].startBeacon,r.units[o].endBeacon),t.insertBefore(r.units[e.id].endBeacon,r.units[o].endBeacon)):t.insertBefore(r.units[e.id].main,r.units[o].endBeacon),!0))()||(r.units[e.id].startBeacon?(t.appendChild(r.units[e.id].startBeacon),t.appendChild(r.units[e.id].endBeacon)):t.appendChild(r.units[e.id].main)))}},h2=n=>e=>r=>t=>()=>{var a,u,o=r.id;if(t.scopes[r.scope]||(t.scopes[r.scope]=[]),t.scopes[r.scope].push(o),!n(r.parent)(()=>()=>t.hydrating&&e&&(a=t.allBeacons[r.id])&&(u=t.allBeacons[`${r.id}%-%`])?(t.units[o]={listeners:{},parent:r.parent,scope:r.scope,pos:r.pos,dynFamily:r.dynFamily,startBeacon:a,endBeacon:u},a.$dekuId=o,u.$dekuId=o,!0):!1)()){const c=document.createComment(`%-%${r.id}`),l=document.createComment(`%-%${r.id}%-%`);t.units[o]={listeners:{},parent:r.parent,dynFamily:r.dynFamily,scope:r.scope,pos:r.pos,startBeacon:c,endBeacon:l},c.$dekuId=o,l.$dekuId=o}},x2=n=>e=>()=>e.units[n]&&e.units[n].dynFamily?e.units[n].dynFamily:(()=>{throw new Error(`No dyn family for ${n}`)})(),y2=n=>e=>()=>e.units[n]&&e.units[n].main&&e.units[n].main.parentNode&&e.units[n].main.parentNode.$dekuId?e.units[n].main.parentNode.$dekuId:e.units[n]&&e.units[n].startBeacon&&e.units[n].startBeacon.parentNode&&e.units[n].startBeacon.parentNode.$dekuId?e.units[n].startBeacon.parentNode.$dekuId:(()=>{throw new Error(`No parent information for ${n}`)})(),Od=n=>e=>()=>e.units[n]&&e.units[n].scope?e.units[n].scope:(()=>{throw new Error(`No scope information for ${n}`)})(),g2=n=>e=>r=>t=>()=>{var a,u=r.id;if(t.scopes[r.scope]||(t.scopes[r.scope]=[]),t.scopes[r.scope].push(u),!n(r.parent)(()=>()=>t.hydrating&&e&&(a=document.documentElement.querySelector(`[data-deku-ssr="${u}"]`))?(t.units[u]={listeners:{},pos:r.pos,parent:r.parent,scope:r.scope,dynFamily:r.dynFamily,main:a},a.$dekuId=u,!0):!1)()){let i=null;n(r.ns)(l=>()=>(i=l,!0))();const c=i===null?document.createElement(r.tag):document.createElementNS(i,r.tag);t.units[u]={listeners:{},parent:r.parent,pos:r.pos,scope:r.scope,dynFamily:r.dynFamily,main:c},c.$dekuId=u}},w2=n=>e=>r=>t=>a=>()=>{var u=t.id,o;if(a.scopes[t.scope]||(a.scopes[t.scope]=[]),a.scopes[t.scope].push(u),!n(t.parent)(c=>()=>{if(a.hydrating&&e&&(o=document.documentElement.querySelector(`[data-deku-ssr="${c}"]`))){for(var l=0;l<o.childNodes.length;l++){const m=u.split("@-@");if(o.childNodes[l].nodeType===8&&o.childNodes[l].nodeValue===m[0]){var s=l===0||o.childNodes[l-1].nodeType!==3;s&&l!==0?o.insertBefore(document.createTextNode(""),o.childNodes[l]):s?o.prepend(document.createTextNode("")):l=l-1;break}}const p=o.childNodes[l];return a.units[u]={main:p,pos:t.pos,parent:t.parent,scope:t.scope,dynFamily:t.dynFamily},p.$dekuId=u,!0}return!1})()){const c=document.createTextNode("");a.units[u]={main:c,parent:t.parent,scope:t.scope,pos:t.pos,dynFamily:t.dynFamily},c.$dekuId=u}};function b2(){return{units:{},scopes:{},allBeacons:{}}}const $2=n=>e=>r=>()=>{if(r.units[e.id]){var t=e.id,a=e.value;r.hydrating&&n&&!r.units[t]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${t}"]`))&&(r.units[t]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(t)),r.units[t].main.tagName==="INPUT"&&e.key==="value"||r.units[t].main.tagName==="TEXTAREA"&&e.key==="value"?r.units[t].main.value=a:r.units[t].main.tagName==="INPUT"&&e.key==="checked"?r.units[t].main.checked=a==="true":e.key==="disabled"?r.units[t].main.disabled=a==="true":r.units[t].main.setAttribute(e.key,a)}},_2=n=>e=>r=>()=>{if(r.units[e.id]){var t=e.id,a=e.value;if(r.hydrating&&n&&!r.units[t]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${t}"]`))&&(r.units[t]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(t)),e.key==="@self@")a(r.units[t].main)();else{r.units[t].listeners[e.key]&&r.units[t].main.removeEventListener(e.key,r.units[t].listeners[e.key]);var u=o=>a(o)();r.units[t].main.addEventListener(e.key,u),r.units[t].listeners[e.key]=u}}},O2=n=>e=>r=>()=>{if(r.units[e.id]){var t=e.id;r.hydrating&&n&&!r.units[t]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${t}"]`))&&(r.units[t]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(t)),r.units[t].main.removeAttribute(e.key)}},k2=n=>e=>()=>{if(e.units[n.id]){var r=n.id;e.units[r].main.nodeValue=n.text}},S2=n=>e=>r=>t=>a=>()=>{var u,o,i=t.id,c=t.html,l=t.dynFamily,s=t.verb,p=t.cache,m=t.parent,v=t.scope,f=t.pxScope;const d=n(t.parent)(()=>()=>a.hydrating&&e&&(u=document.documentElement.querySelector(`[data-deku-ssr="${i}"]`))?(a.units[i]={listeners:{},pos:t.pos,scope:v,parent:m,main:u,dynFamily:l},u.$dekuId=i,!0):!1)();if(!d){const b=Object.entries(p);for(var h=0;h<b.length;h++){const O=b[h][0];b[h][1]===!0?c=c.replace(s+O+s,'data-deku-attr-internal="'+O+'"'):c=c.replace(s+O+s,'<span style="display:contents;" data-deku-elt-internal="'+O+'"></span>')}o=document.createElement("div"),o.innerHTML=c.trim(),a.units[i]={listeners:{},pos:t.pos,scope:v,dynFamily:l,parent:m,main:o.firstChild},o.firstChild.$dekuId=i}a.scopes[v]||(a.scopes[v]=[]),a.scopes[v].push(i),o||(o=u),o.querySelectorAll("[data-deku-attr-internal]").forEach(function(b){var O=b.getAttribute("data-deku-attr-internal");const g=O+"@!%"+f;a.units[g]={listeners:{},main:b,scope:v},a.scopes[v].push(g)}),o.querySelectorAll("[data-deku-elt-internal]").forEach(function(b){var O=b.getAttribute("data-deku-elt-internal");const g=O+"@!%"+f;a.units[O+"@!%"+f]={listeners:{},main:b,scope:v},a.scopes[v].push(g)}),d||a.units[i].main.remove()},T2=n=>e=>()=>{var r=n.id;e.units[r]={main:n.root},n.root.$dekuId=r},kd=n=>e=>r=>t=>()=>{const a=(f,d,h)=>{if(t.units[f].startBeacon){var b=t.units[f].startBeacon,O=b.nextSibling;for(t.units[d].main.insertBefore(b,h),b=O;b&&b!==t.units[f].endBeacon;)O=b.nextSibling,t.units[d].main.insertBefore(b,h),b=O}else t.units[d].main.insertBefore(t.units[f].main,h)},u=[];u.push(r);for(var o=0;o<u.length;o++){const f=u[o],d=f.id,h=f.parent;t.units[d].containingScope=f.scope;var i=void 0;e(f.pos)(O=>()=>(i=O,!0))(),i===void 0&&(i=Number.MAX_VALUE);const b=t.units[h].main.childNodes;for(var c=0,l=!1,s=0;c<b.length;){var p;if(p=b[c].$dekuId){if(e(f.dynFamily)(g=>()=>l?!1:t.units[p].endBeacon===b[c]&&g===p?(t.units[d].pos=n(s),a(d,h,b[c]),!0):!1)()){l=!0;break}if(t.units[p].dynFamily!==f.dynFamily){c++;continue}if(l){c++;continue}s===i?(a(d,h,b[c]),s++,l=!0):t.units[p].endBeacon!==b[c]&&(t.units[p].pos=n(s),s++)}c++}if(l)return;if(t.units[d].main)t.units[h].main.appendChild(t.units[d].main);else{var m=t.units[d].startBeacon,v=m.nextSibling;for(t.units[h].main.appendChild(m),m=v;m&&m!==t.units[d].endBeacon;)v=m.nextSibling,t.units[h].main.appendChild(m),m=v}}},P2=n=>e=>()=>{if(e.units[n.id]){var r=n.id;if(e.units[r].containingScope&&!n.scopeEq(e.units[r].containingScope)(n.scope))return;if(e.units[r].main)e.units[r].main.remove();else{const u=document.createElement("div");var t=e.units[r].startBeacon,a=t.nextSibling;for(u.appendChild(t),t=a;t&&t!==e.units[r].endBeacon;)a=t.nextSibling,u.appendChild(t),t=a;t===e.units[r].endBeacon&&u.appendChild(t)}}},A2=n=>e=>()=>e.units[n]!==void 0,Sd=n=>e=>()=>{e.units[n.id]&&(e.units[n.id].unsubscribe&&e.units[n.id].unsubscribe(),delete e.units[n.id])},E2=Sd;function I2(n,e){return e.includes(n)}const q2=function(n){return function(e){return function(r){return(r|0)===r?n(r):e}}},ae=function(n){return n},F2=function(n){return function(e){return Math.pow(n,e)|0}},C2=isFinite,B2=Math.floor,ri=function(n){return function(e){return Math.pow(n,e)}},Td=function(n){return function(e){return n%e}},D2=Math.round,ds=Math.sin;var ro=3.141592653589793,ms=jx(wp),hs=Vx(wp),Pd=function(){return q2($.create)(_.value)}(),Ad=function(n){return C2(n)?n>=ae(ms)?ms:n<=ae(hs)?hs:bp(0)(Pd(n)):0},R2=function(n){return Ad(D2(n))},Ed=function(n){return Ad(B2(n))},N2=function(n){return function(e){return I2(n,e)}},Id=Pe(rr),z2=se(ma),L2=S(gn),M2=Xe(fy(Sy(Pp))),W2=Xe(g$),qd=wa(),U2=yr(lo),ul=Qp(Ka),xs=Wp(Tn),H2=Zp(ul)(Ip()(fi({reflectSymbol:function(){return"instructions"}})(xs)()(fi({reflectSymbol:function(){return"newMap"}})(xs)()(Ap)))),j2=Si(ul),V2=ff(sf),ys=ba(),G2=Dt(co),Q2=Cf(ul),Io=Ie(Je),Y2=Jn(fp),K2=Ie(Ti),X2=Be(je()),J2=An(Tn),Z2=vr(gn)(tr),sa=function(n){return function(e){return n instanceof $?e(n.value0):L2(!1)}},n_=function(n){return function(e){return function(){var t=Od(n.id)(e)(),a=y2(n.id)(e)(),u=x2(n.id)(e)(),o={scope:t,parent:a,dynFamily:u,id:n.id,deferralPath:wn.value,pos:new $(n.pos),ez:!1,raiseId:M2,ctor:function(i){return i}(W2)};return qd(kd($.create)(sa)(o))(e)()}}},e_=function(n){var e=Pe(n);return function(r){var t=r.Monad0(),a=t.Bind1(),u=Zn(a),o=Ie(r),i=V2(a),c=vr(t.Applicative0())(he);return function(l){return function(s){return function(p){var m=function(v){return u(o(V(l)))(function(f){var d=function(g){return g instanceof E&&g.value1 instanceof wn?new E(g.value0+1|0,wn.value):g instanceof E?new E(g.value0,d(g.value1)):g},h=new $(d(p)),b=new $(p),O=Kn(H2(b)(h))(f)(function(g){return function(G){return{newMap:j2(g),instructions:ki(G)}}});return i(e(o(ie(ys(O.newMap))(l))))(function(){return c(G2(ys(O.instructions)([])))(s)})})};return m}}}}},r_=e_(Ee)(Je),t_=function(n){var e=Pe(n);return function(r){var t=Ie(r);return function(a){return function(u){return function(o){return function(i){return e(t(ie(Kn(Q2)(u)(function(c){if(c instanceof _)return new $([o]);if(c instanceof $)return new $(z2(c.value0)([o]));throw new Error("Failed pattern match at Deku.Interpret (line 395, column 24 - line 397, column 36): "+[c.constructor.name])}))(a)))}}}}}},a_=t_(Ee)(Je),u_=function(n){return function(e){return function(r){return Lr(Ri)(qd(n))(e)(r)}}},o_=function(n){return function(e){return function(r){return function(t){return function(a){return function(u){var o=function(){var l=Io(J(_.value))(),s=Y2(a.raiseId)(function(){var d=Kn(Yn)(l);return function(h){return Id(K2(d($.create(h))))}}()),p=u_(a.ctor)({dynFamily:a.dynFamily,ez:a.ez,deferralPath:a.deferralPath,parent:new $(a.parent),pos:a.pos,raiseId:s,scope:a.scope})(n),m=Io(_a)(),v=Io(sr(X2(p)(m.event))(e))();m.push(J2)();var f=Io(V(l))();return Z2(f)(function(d){return e(n.associateWithUnsubscribe(function(h){return{unsubscribe:v,id:h}}(d)))})()},i=kd(r)(t)(a)(u);return function(){var l=A2(a.id)(u)();if(l){var s=Od(a.id)(u)();if(s instanceof Xu)return i();if(s instanceof tt&&a.scope instanceof tt){var p=N2(s.value0)(a.scope.value0);return p?i():o()}return o()}return o()}}}}}}},i_=function(n){return function(e){return function(r){var t={ids:function(){var u=V(n)();return Id(ie(U2(1))(n))(),u},associateWithUnsubscribe:d2,deferPayload:a_(e),forcePayload:r_(e)(r),makeElement:g2(sa)(!1),makeDynBeacon:h2(sa)(!1),attributeParent:m2(sa),makeRoot:T2,makeText:w2(sa)(!1)(pn(w)),makePursx:S2(sa)(!1)(pn(w)),setProp:$2(!1),setCb:_2(!1),unsetAttribute:O2(!1),setText:k2,sendToPos:n_,removeDynBeacon:E2,deleteFromCache:Sd,giveNewParent:function(a){return o_(t)(r)($.create)(sa)(a)},disconnectElement:P2};return t}}},Fd=function(n){var e=k(n);return{map:function(r){return function(t){return function(a){return e(function(u){return new q(r(u.value0),u.value1)})(t(a))}}}}},Cd=function(n){return{Applicative0:function(){return Dd(n)},Bind1:function(){return Bd(n)}}},Bd=function(n){var e=Zn(n.Bind1());return{bind:function(r){return function(t){return function(a){return e(r(a))(function(u){var o=t(u.value0);return o(u.value1)})}}},Apply0:function(){return ol(n)}}},ol=function(n){var e=Fd(n.Bind1().Apply0().Functor0());return{apply:fo(Cd(n)),Functor0:function(){return e}}},Dd=function(n){var e=S(n.Applicative0());return{pure:function(r){return function(t){return e(new q(r,t))}},Apply0:function(){return ol(n)}}},c_=function(n){var e=S(n.Applicative0()),r=Cd(n);return{state:function(t){return function(a){return e(t(a))}},Monad0:function(){return r}}},f_=function(n){return function(e){var r=n(e);return r.value0}},gs=Zn(Nt),Aa=Ie(Je),l_=Oa(je())(U)(U),s_=vr(gn)(Ue),v_=it(gn)(he),p_=k(ya),d_=Pe(Ee),m_=function(n){return function(e){return function(){var t=b2(),a=Aa(J(0))(),u=Aa(J(Ja))(),o=function(s){return s(t)},i=P$(n)(e)(i_(a)(u)(o)),c=Aa(_a)(),l=Aa(sr(l_(i)(c.event))(o))();return c.push(w)(),function(){var p=Aa(V(u))();return s_(p)(v_(o))(),Aa(l)(),t}}}},h_=function(n){return function(){var r=gs(gs(qi)(Sw))(Ow)();return pn(id(mi("Could not find element")))(Kn(m_)(n))(p_(kw)(r))()}},x_=function(n){return d_(h_(n))},Rd=Ir(xe),to=Be(je()),y_=k(Hn(U)),g_=k(U),ws=k(rr),bs=xa(ha),w_=vr(Le)(tr),b_=S(Le),I={pursxToElement:function(n){return function(e){return function(r){return{cache:Uf,element:new H(function(t){return function(a){return Xn(function(u){return Rd})}})}}}}},il=function(n){return n.pursxToElement},cl=function(n){var e=function(r){return r instanceof H?r.value0:function(t){return function(a){return Xn(function(u){return Rd})}}};return e(n)},Br=function(){return function(n){var e=il(n);return function(r){var t=Ju(r);return function(a){var u=Ni(a)();return{pursxToElement:function(o){return function(i){return function(c){var l=e(o)(x.value)(c),s=cl(l.element),p=function(m){return function(v){return cn(function(f){return function(d){return function(h){var b=s(m)(v),O=t(x.value)+("@!%"+o);return function(){return h(to(y_(Y0)(u(x.value)(c)))(g_(function(G){return function(R){return G(ad(v)(O)(R))}})(f)))(),d(v.deferPayload(m.deferralPath)(v.deleteFromCache({id:O})))(),h(to(b)(f))()}}}})}};return{cache:Zu(t(x.value))(!0)(l.cache),element:new H(p)}}}}}}}}},$_=function(){return function(n){var e=il(n);return function(r){return function(t){return function(a){var u=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=ws(bs)(c.ids)(),f=ws(bs)(c.ids)();i.raiseId(v)();var d=e(f)(x.value)(a),h=cl(d.element),b=h(i)(c);return s(c.makePursx({id:v,parent:i.parent,cache:d.cache,dynFamily:i.dynFamily,pos:i.pos,pxScope:f,scope:i.scope,html:t,verb:r}))(),w_(i.parent)(function(O){return s(c.attributeParent({id:v,parent:O,pos:i.pos,dynFamily:i.dynFamily,ez:!1}))})(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:v})))(),p(to(b)(l))()}}}})}},o=new H(u);return o}}}}},__=function(n){return function(e){return function(r){return Lr(Ri)(n)(e)(r)}}},z=function(){return function(n){var e=il(n);return function(r){var t=Ju(r);return function(a){var u=Ni(a)();return{pursxToElement:function(o){return function(i){return function(c){var l=e(o)(x.value)(c);return{cache:Zu(t(x.value))(!1)(l.cache),element:new H(function(s){return function(p){return cn(function(m){return function(v){return function(f){var d=cl(l.element),h=to(d(s)(p))(m),b=to(__(function(O){return O}(u(x.value)(c)))({parent:new $(t(x.value)+("@!%"+o)),scope:s.scope,raiseId:function(O){return b_(w)},pos:s.pos,ez:!1,deferralPath:s.deferralPath,dynFamily:_.value})(p))(m);return function(){return v(p.deferPayload(s.deferralPath)(p.deleteFromCache({id:t(x.value)+("@!%"+o)})))(),f(Rf([b,h]))()}}}})}})}}}}}}}}},O_=$_(),k_={reflectType:function(){return"~"}},Mn=function(n){var e=Ju(n);return function(r){var t=Ju(r);return function(){return function(){return function(a){var u=O_(a);return function(o){return function(i){return u(t(o))(e(i))}}}}}}},xn=function(n){var e=Mn(n)(k_)()();return function(){return function(){return function(r){return e(r)(x.value)}}}},S_=b$()(),T_=An(Ab),P_=wi(W0),A_=U0()(),we=function(){return function(){return{defaults:Kn(S_)}}},E_=function(n){return n.defaults},be={convertRecordOptions:function(n){return function(e){return function(r){return T_}}}},Nd=function(n){return n.convertRecordOptions},Ze=function(n){return n.convertOptionsWithDefaults},$e=function(){return function(n){var e=Nd(n);return{convertOptions:function(r){return function(t){return Eb(e(r)(x.value)(t))}}}}},I_=function(n){return n.convertOptions},On=function(n){var e=I_(n);return function(r){var t=E_(r);return{convertOptionsWithDefaults:function(a){return function(u){var o=t(u),i=e(a);return function(c){return o(i(c))}}}}}},q_=function(n){return n.convertOption},j=function(n){var e=Nd(n);return function(r){var t=q_(r);return function(){return function(){return function(){return function(a){var u=A_(a),o=Ni(a)();return{convertRecordOptions:function(i){return function(c){return function(l){return P_(u(x.value)(t(i)(x.value)(o(x.value)(l))))(e(i)(x.value)(l))}}}}}}}}}},F_=function(){return function(){return function(){return function(n){return function(e){return function(r){return lx(r.type)(n)?df(r.type)(n)(r.value):e(r)}}}}}},C_=F_()()(),Yi=function(){return function(n){var e=da(n);return function(r){return function(t){return{type:e(r),value:t}}}}},B_=function(n){return Wa("Data.Variant: pattern match failure ["+(n.type+"]"))},D_=function(){return function(){return function(){return function(n){return C_(n)(B_)}}}},R_=function(){var n=lg(_g);return function(e){return sg(n(e))}}(),N_=function(n){return Ig(n)},z_=function(n){var e=Jp(n);return function(r){return function(t){return e(r)(w)(t)}}},L_=Ja,M_=function(n){var e=Si(n);return function(r){return function(t){return e(r)(t)}}},pt=w,zd=function(n){return n.toInt},W_=function(n){var e=zd(n);return function(r){return e(pt)}},dt={toInt:function(n){return 8}},fu={toInt:function(n){return 7}},lu={toInt:function(n){return 6}},$r={toInt:function(n){return 5}},Lt={toInt:function(n){return 4}},Mt={toInt:function(n){return 3}},Wt={toInt:function(n){return 2}},Ut={toInt:function(n){return 1}},Ht={toInt:function(n){return 0}},Mr={Nat0:function(){return fu},Nat1:function(){return dt}},Wr={Nat0:function(){return lu},Nat1:function(){return dt}},Ur={Nat0:function(){return $r},Nat1:function(){return dt}},Hr={Nat0:function(){return Lt},Nat1:function(){return dt}},mt={Nat0:function(){return Lt},Nat1:function(){return $r}},jr={Nat0:function(){return Mt},Nat1:function(){return dt}},ht={Nat0:function(){return Mt},Nat1:function(){return $r}},Vr={Nat0:function(){return Wt},Nat1:function(){return dt}},xt={Nat0:function(){return Wt},Nat1:function(){return $r}},Gr={Nat0:function(){return Ut},Nat1:function(){return dt}},yt={Nat0:function(){return Ut},Nat1:function(){return $r}},Qr={Nat0:function(){return Ht},Nat1:function(){return dt}},gt={Nat0:function(){return Ht},Nat1:function(){return $r}};const ao=Math.random;var Ld=function(n){return function(e){return function(){var t=ao(),a=(ae(e)-ae(n)+1)*t+ae(n);return Ed(a)}}},U_=$f(_f),H_=$p(),j_=function(n){return n},V_=1,fl=2147483647,G_=function(){return fl-1|0}(),Md=function(n){var e=function(r){return function(t){return function(a){var u=t-r|0,o=U_(a)(u),i=o<r;return i?o+t|0:o}}};return e(V_)(G_)(n)},Q_=0,Y_=48271,K_=function(n){return function(e){return H_(Pd(Td(ae(Y_)*ae(e)+ae(n))(ae(fl))))}},X_=K_(Q_),J_=c_(ga),Z_=t1(J_),nO=Bd(ga),eO=Fd(bf),rO=xp(Rt),tO=yr(Rt),aO=ab(),uO=function(n){return n},Wd=function(){var n=function(e){return new q(j_(e.newSeed),function(){var r={};for(var t in e)({}).hasOwnProperty.call(e,t)&&(r[t]=e[t]);return r.newSeed=X_(e.newSeed),r}())};return Z_(n)}(),Ud=eO,Wu=k(Ud),oO=Wu(function(n){return ae(n)/ae(fl)})(Wd),Hd=function(n){return f_(uO(n))},jd=nO,iO=Zn(jd),Vd=ol(ga),cO=ne(Vd),$s=function(n){return function(e){var r=ae(e),t=ae(n),a=function(i){return t+Td(i)(r-t+1)},u=Wu(ae)(Wd),o=cO(Wu(tO)(u))(Wu(rO(2))(u));return Wu(function(i){return Ed(a(i))})(o)}},fO=function(n){return function(e){var r=n<=e;return r?$s(n)(e):$s(e)(n)}},Gd=Dd(ga),lO=S(Gd),sO=function(n){return iO(fO(0)(tb(n)-1|0))(function(e){return lO(aO(n)(e))})},vO=function(n){return n.arbitrary},pO={arbitrary:oO},dO=Hp(),mO=k(Ce),hO=If,_s=function(n){return n},Os=function(n){var e=zd(n);return function(){return function(r){return function(t){return dO(r)(e(t))}}}},Qd=function(n){var e=W_(n);return function(r){var t=e(x.value),a=function(){return t===0?[]:ct(0)(t-1|0)}();return mO(r)(a)}},Bt=[],_e=function(n){return function(e){return function(r){return ki(e)(r)}}},ll={first:function(n){return function(e){return new q(n(e.value0),e.value1)}},second:k(Mp),Profunctor0:function(){return au}},Yd=function(n){return n.second},xO=function(n){return n.first},ks=wa(),yO=function(n){return function(e){return function(r){var t=Yf(r);return function(a){return t(n)(e)(a)}}}},gO=function(){return function(){return function(n){return yO(ks)(ks)(n)}}},wO=gO()(),bO=function(){return function(){return function(n){return wO(n)}}},$O=function(n){return function(e){var r=Yf(e.Profunctor0()),t=xO(e);return function(a){return r(n)(function(u){return u.value1(u.value0)})(t(a))}}},_O=function(n){return function(e){return function(r){return $O(function(t){return new q(n(t),function(a){return e(t)(a)})})(r)}}},OO=function(n){var e=Ni(n)(),r=w$(n)()();return function(){return function(){return function(t){return function(a){return _O(e(t))(Kn(r(t)))(a)}}}}},kO=function(n){return n},SO=ba(),TO=function(n){return SO(u1(n))};function Ss(n){return Object.prototype.toString.call(n).slice(8,-1)}var PO=function(){function n(e,r){this.value0=e,this.value1=r}return n.create=function(e){return function(r){return new n(e,r)}},n}(),AO=qr,EO=function(n){var e=J$(c1(n));return function(r){return e(R_(r))}},Kd=function(n){var e=S(sd(n)),r=EO(n);return function(t){return function(a){return Ss(a)===t?e(AO(a)):r(new PO(t,Ss(a)))}}},IO=function(n){return Kd(n)("String")},qO=OO({reflectSymbol:function(){return"o"}})()(),FO={reflectSymbol:function(){return"2x"}},CO={reflectSymbol:function(){return"on"}},BO={reflectSymbol:function(){return"off"}},DO=bO()()(au),_r=Yi(),RO=S(jn(Z)),NO=Pf(),zO=_r({reflectSymbol:function(){return"onOff"}}),Xd=function(){function n(){}return n.value=new n,n}(),Jd=function(){function n(){}return n.value=new n,n}(),LO=function(){function n(){}return n.value=new n,n}(),MO=function(){function n(){}return n.value=new n,n}(),Zd=function(){function n(){}return n.value=new n,n}(),WO=function(){function n(){}return n.value=new n,n}(),UO=function(){function n(){}return n.value=new n,n}(),HO=function(n){return n},jO=function(n){return n},VO=function(n){return n},GO=function(n){return n},QO=function(n){return n},YO=function(n){return n},KO=function(n){return n},XO=function(n){return n},JO=function(n){return n},ZO=function(n){return n},nm=function(){function n(){}return n.value=new n,n}(),nk=function(){function n(){}return n.value=new n,n}(),ek=function(){function n(){}return n.value=new n,n}(),em=function(){function n(){}return n.value=new n,n}(),rk=function(){function n(){}return n.value=new n,n}(),sc=function(n){return n},gu=function(n){return n},tk=function(n){return n},bo=function(n){return n},Ki={toAudioOnOff:An(Tn)},su=function(n){return n.toAudioParameter},ak=function(n){return n.toAudioOnOff},uk=function(){return Ci.value}(),rm=function(){return kO(function(){var n=qO(x.value)(ll);return function(e){return DO(n(e))}}())},ok=qr,ik=function(){var n=_r({reflectSymbol:function(){return"unit"}})(x.value);return function(e){return bo(n(e))}}(),ck=function(n){return{toAudioParameter:function(e){return ik(e)}}},fk=function(n){return{toAudioParameter:function(){var e=su(ck());return function(r){return e(tk(function(t){return{u:t}}(r)))}}()}},lk=function(){return _r(FO)(x.value)(w)}(),tm=function(){var n=_r({reflectSymbol:function(){return"sudden"}})(x.value);return function(e){return bo(n(e))}}(),sk={toAudioParameter:tm},am={toAudioParameter:function(n){return tm({n})}},Ts=function(){return _r({reflectSymbol:function(){return"step"}})(x.value)(w)}(),um=function(){return _r(CO)(x.value)(w)}(),sl={x:um,o:0},nn=function(){return RO(NO(zO(x.value)(sl)))},vk=function(){return _r(BO)(x.value)(w)}(),pk=function(){var n=_r({reflectSymbol:function(){return"numeric"}})(x.value);return function(e){return bo(n(e))}}(),jt={toAudioParameter:pk},uo=function(){return _r({reflectSymbol:function(){return"linear"}})(x.value)(w)}(),dk=function(){return _r({reflectSymbol:function(){return"exponential"}})(x.value)(w)}(),mk=function(){var n=_r({reflectSymbol:function(){return"envelope"}})(x.value);return function(e){return bo(n(e))}}(),Dr={toAudioParameter:mk},hk=function(){var n=_r({reflectSymbol:function(){return"cancel"}})(x.value);return function(e){return bo(n(e))}}(),xk={toAudioParameter:hk},yk=Yi(),gk=yk({reflectSymbol:function(){return"realImg"}}),Cn=An(Tn),Ps=ub(F0),As=wa(),Vt=$e(),Gt=j(be),Qt=we()(),om={reflectSymbol:function(){return"buffer"}},Xi={reflectSymbol:function(){return"frequency"}},wk=function(){function n(){}return n.value=new n,n}(),bk=function(){function n(){}return n.value=new n,n}(),$k=function(){function n(){}return n.value=new n,n}(),_k=function(){function n(){}return n.value=new n,n}(),Ok=function(){function n(){}return n.value=new n,n}(),kk=function(){function n(){}return n.value=new n,n}(),Sk=function(){function n(){}return n.value=new n,n}(),Tk=function(){function n(){}return n.value=new n,n}(),Pk=function(){function n(){}return n.value=new n,n}(),Ak=function(){function n(){}return n.value=new n,n}(),Ek=function(){function n(){}return n.value=new n,n}(),Ik=function(){function n(){}return n.value=new n,n}(),qk=function(){function n(){}return n.value=new n,n}(),Fk=function(){function n(){}return n.value=new n,n}(),vl=function(n){return{toPeriodicOscSpec:function(e){return gk(x.value)({real:_s(e.value0),img:_s(e.value1)})}}},im={toInitializeTriangleOsc:function(n){return ZO(function(e){return{frequency:e}}(n))}},Ck={toInitializeStereoPanner:function(n){return JO(function(e){return{pan:e}}(n))}},pl={toInitializeSquareOsc:function(n){return XO(function(e){return{frequency:e}}(n))}},Ji={toInitializeSinOsc:function(n){return KO(function(e){return{frequency:e}}(n))}},Bk={toInitializeSawtoothOsc:function(n){return YO(function(e){return{frequency:e}}(n))}},Dk={toInitializeRecorder:function(n){return HO(function(e){return{cb:e}}(n))}},cm={toInitializeMicrophone:function(n){return jO(function(e){return{microphone:e}}(n))}},Rk=function(n){return function(e){return{toInitializeIIRFilter:function(r){return function(t){return function(a){return{feedforward:Ps(As(r.value0)),feedback:Ps(As(r.value1))}}}}}}},an={toInitializeGain:function(n){return QO(function(e){return{gain:e}}(n))}},Nk={toInitializeConvolver:function(n){return VO(function(e){return{buffer:e}}(n))}},fm={toInitializeConstant:function(n){return GO(function(e){return{offset:e}}(n))}},zk={convertOption:function(n){return function(e){return Cn}}},dl={convertOption:function(n){return function(e){return Cn}}},Lk={convertOption:function(n){return function(e){return Cn}}},Mk={convertOption:function(n){return function(e){return $.create}}},Wk={convertOption:function(n){return function(e){return Cn}}},ml={convertOption:function(n){return function(e){return Cn}}},Uk={convertOption:function(n){return function(e){return Cn}}},Hk={convertOption:function(n){return function(e){return Cn}}},jk={convertOption:function(n){return function(e){return Cn}}},Vk={convertOption:function(n){return function(e){return Cn}}},Gk={convertOption:function(n){return function(e){return Cn}}},Qk={convertOption:function(n){return function(e){return Cn}}},Yk={convertOption:function(n){return function(e){return Cn}}},Kk={convertOption:function(n){return function(e){return Cn}}},lm={convertOption:function(n){return function(e){return Cn}}},Zi={convertOption:function(n){return function(e){return Cn}}},sm={convertOption:function(n){return function(e){return Cn}}},vm={convertOption:function(n){return function(e){return Cn}}},pm={convertOption:function(n){return function(e){return Cn}}},Xk={convertOption:function(n){return function(e){return Cn}}},Jk={convertOption:function(n){return function(e){return Cn}}},Zk={convertOption:function(n){return function(e){return Cn}}},dm={convertOption:function(n){return function(e){return Cn}}},nS={convertOption:function(n){return function(e){return Cn}}},mm={convertOption:function(n){return function(e){return Cn}}},$o={convertOption:function(n){return function(e){return Cn}}},vu={convertOption:function(n){return function(e){return Cn}}},eS={convertOption:function(n){return function(e){return Cn}}},hm={convertOption:function(n){return function(e){return Cn}}},rS=function(n){return n.toPeriodicOscSpec},hl=function(n){var e=rS(n);return{convertOption:function(r){return function(t){return e}}}},xm=function(n){return n.toInitializeWaveShaper},tS=function(n){return n.toInitializeTriangleOsc},aS=function(n){return n.toInitializeStereoPanner},uS=function(n){return n.toInitializeSquareOsc},oS=function(n){return n.toInitializeSinOsc},iS=function(n){return n.toInitializeSawtoothOsc},cS=function(n){return n.toInitializeRecorder},ym=function(n){return n.toInitializePlayBuf},fS=function(n){return n.toInitializePeriodicOsc},lS=function(n){return n.toInitializePeaking},sS=function(n){return n.toInitializeNotch},vS=function(n){return n.toInitializeMicrophone},pS=function(n){return n.toInitializeLowshelf},gm=function(n){return n.toInitializeLowpass},wm=function(n){return n.toInitializeLoopBuf},dS=function(n){return n.toInitializeIIRFilter},mS=function(n){return n.toInitializeHighshelf},bm=function(n){return n.toInitializeHighpass},hS=function(n){return n.toInitializeGain},xS=function(n){return n.toInitializeDynamicsCompressor},$m=function(n){return n.toInitializeDelay},yS=function(n){return n.toInitializeConvolver},gS=function(n){return n.toInitializeConstant},_m=function(n){return n.toInitializeBandpass},Om=function(n){return n.toInitializeAllpass},wS={oversample:lk},bS=function(n){var e=Ze(n);return{toInitializeWaveShaper:function(r){return e(wk.value)(wS)(r)}}},$S={toInitializeWaveShaper:function(){var n=xm(bS(On(Vt(Gt(zk)()()()({reflectSymbol:function(){return"curve"}})))(Qt)));return function(e){return n(function(r){return{curve:r}}(e))}}()},_S=function(){return{bufferOffset:0,playbackRate:1,duration:_.value}}(),xl=function(n){var e=Ze(n);return{toInitializePlayBuf:function(r){return e(bk.value)(_S)(r)}}},pu={toInitializePlayBuf:function(){var n=ym(xl(On(Vt(Gt(dl)()()()(om)))(Qt)));return function(e){return n(function(r){return{buffer:r}}(e))}}()},OS={},yl=function(n){var e=Ze(n);return{toInitializePeriodicOsc:function(r){return e($k.value)(OS)(r)}}},kS={q:1,gain:0},SS=function(n){var e=Ze(n);return{toInitializePeaking:function(r){return e(_k.value)(kS)(r)}}},TS={q:1},PS=function(n){var e=Ze(n);return{toInitializeNotch:function(r){return e(Ok.value)(TS)(r)}}},AS={gain:0},ES=function(n){var e=Ze(n);return{toInitializeLowshelf:function(r){return e(kk.value)(AS)(r)}}},IS={q:1},km=function(n){var e=Ze(n);return{toInitializeLowpass:function(r){return e(Sk.value)(IS)(r)}}},Sm={toInitializeLowpass:function(){var n=gm(km(On(Vt(Gt(lm)()()()(Xi)))(Qt)));return function(e){return n(function(r){return{frequency:r}}(e))}}()},qS=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:_.value}}(),nc=function(n){var e=Ze(n);return{toInitializeLoopBuf:function(r){return e(Tk.value)(qS)(r)}}},ee={toInitializeLoopBuf:function(){var n=wm(nc(On(Vt(Gt(Zi)()()()(om)))(Qt)));return function(e){return n(function(r){return{buffer:r}}(e))}}()},FS={gain:0},CS=function(n){var e=Ze(n);return{toInitializeHighshelf:function(r){return e(Pk.value)(FS)(r)}}},BS={q:1},Tm=function(n){var e=Ze(n);return{toInitializeHighpass:function(r){return e(Ak.value)(BS)(r)}}},gl={toInitializeHighpass:function(){var n=bm(Tm(On(Vt(Gt(dm)()()()(Xi)))(Qt)));return function(e){return n(function(r){return{frequency:r}}(e))}}()},DS=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),RS=function(n){var e=Ze(n);return{toInitializeDynamicsCompressor:function(r){return e(Ek.value)(DS)(r)}}},NS={maxDelayTime:1},Pm=function(n){var e=Ze(n);return{toInitializeDelay:function(r){return e(Ik.value)(NS)(r)}}},du={toInitializeDelay:function(){var n=$m(Pm(On(Vt(Gt(mm)()()()({reflectSymbol:function(){return"delayTime"}})))(Qt)));return function(e){return n(function(r){return{delayTime:r}}(e))}}()},zS={q:1},mu=function(n){var e=Ze(n);return{toInitializeBandpass:function(r){return e(qk.value)(zS)(r)}}},LS={toInitializeBandpass:function(){var n=_m(mu(On(Vt(Gt(vu)()()()(Xi)))(Qt)));return function(e){return n(function(r){return{frequency:r}}(e))}}()},MS={q:1},Am=function(n){var e=Ze(n);return{toInitializeAllpass:function(r){return e(Fk.value)(MS)(r)}}},WS={toInitializeAllpass:function(){var n=Om(Am(On(Vt(Gt(hm)()()()(Xi)))(Qt)));return function(e){return n(function(r){return{frequency:r}}(e))}}()};const US=function(n){return function(){console.error(n)}};var HS=function(n,e,r){var t=0,a;return function(u){if(t===2)return a;if(t===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return t=1,a=r(),t=2,a}},Em=An(Tn),hi=ba(),Uu=wa(),Es=S(gn),y=xa(ha),Im=S(Le),ec=je(),pe=k(Hn(U)),ce=D_()()(),nr=Ir(gr(xe)),Is=B0(),wu=Yi(),Se=S(jn(Z)),jS=Be(ec),VS=ve(Ve(He)),GS=Pe(rr),QS=mo(ye),Oe=Ai(tu(ye)(xe)(ec)),YS=n$()()(),qm=Di(au),KS=k(C0),XS=cb({reflectType:function(){return 0}})()()(),JS=function(){function n(){}return n.value=new n,n}(),Fm={convertOption:function(n){return function(e){return Em}}},Cm={convertOption:function(n){return function(e){return Em}}},ZS=function(n){return n.toInitializeAnalyser},nT=function(n){var e=Be(n);return function(r){return function(t){return function(a){return r(e(a)(t))}}}},de=nT(ec),Vn=function(n){if(n instanceof Xu)return _.value;if(n instanceof tt)return new $(n.value0);throw new Error("Failed pattern match at Ocarina.Control (line 66, column 1 - line 66, column 38): "+[n.constructor.name])},zr=Kb({doLogic:pf,ids:function(n){return function(e){return e.ids}(hi(n))},deferPayload:function(n){return n.deferPayload},forcePayload:function(n){return n.forcePayload},disconnectElement:function(n){return function(e){return n.disconnectXFromY({from:e.id,to:e.parent})}},toElt:function(n){return n}})({fromElt:Uu,connectToParent:function(n){return function(e){return n.connectXToY({from:e.id,to:e.parent})}}}),eT=function(){return{cb:function(n){return Es(Es(w))},fftSize:Zd.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:em.value,channelInterpretation:nm.value}}(),Bm=function(n){var e=Ze(n);return{toInitializeAnalyser:function(r){return e(JS.value)(eT)(r)}}},rT=function(n){var e=vS(n);return function(r){var t=e(r),a=function(u){return function(o){return cn(function(i){return function(c){return function(l){return function(){var p=o.ids();return u.raiseId(y(p))(),c(o.makeMicrophone({id:y(p),parent:u.parent,scope:Vn(u.scope),microphone:t.microphone}))(),c(o.deferPayload(u.deferralPath)(o.deleteFromCache({id:y(p)})))()}}}})}};return new H(a)}},Dm=function(n){return rT(n)},Rm=function(n){return function(e){return function(r){return Lr({doLogic:pf,deferPayload:function(t){return t.deferPayload},forcePayload:function(t){return t.forcePayload},ids:function(t){return function(a){return a.ids}(hi(t))},disconnectElement:function(t){return function(a){return t.disconnectXFromY({from:a.id,to:a.parent})}},toElt:function(t){return t}})(r)(n)(e)}}},tT=function(n){var e=Be(n);return function(r){var t=xa(r);return function(a){return function(u){return function(o){return function(i){return function(c){return function(l){return function(s){return a(e(Rm({parent:new $(t(u)),deferralPath:o,scope:i,raiseId:function(p){return Im(w)}})(c)(l))(s))}}}}}}}}},De=tT(ec)(ha),aT=function(n){var e=ZS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeAnalyser({id:y(v),parent:i.parent,scope:Vn(i.scope),cb:u.cb,fftSize:F2(2)(function(){if(u.fftSize instanceof Xd)return 7;if(u.fftSize instanceof Jd)return 8;if(u.fftSize instanceof LO)return 9;if(u.fftSize instanceof MO)return 10;if(u.fftSize instanceof Zd)return 11;if(u.fftSize instanceof WO)return 12;if(u.fftSize instanceof UO)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 216, column 15 - line 223, column 28): "+[u.fftSize.constructor.name])}()),maxDecibels:u.maxDecibels,minDecibels:u.minDecibels,smoothingTimeConstant:u.smoothingTimeConstant,channelCount:u.channelCount,channelCountMode:function(){if(u.channelCountMode instanceof rk)return"explicit";if(u.channelCountMode instanceof em)return"max";if(u.channelCountMode instanceof ek)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 229, column 29 - line 232, column 40): "+[u.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(u.channelInterpretation instanceof nm)return"speakers";if(u.channelInterpretation instanceof nk)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 233, column 34 - line 235, column 35): "+[u.channelInterpretation.constructor.name])}()}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(pe(function(f){return ce({cb:function(d){return c.setAnalyserNodeCb({id:y(v),cb:d})}})(f)})(t))()}}}})}};return new H(o)}}}},Nm=function(n){var e=aT(n);return function(r){return e(r)(nr)}},uT=function(n){var e=yS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeConvolver({id:y(m),parent:o.parent,scope:Vn(o.scope),buffer:a.buffer}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),De(s)(m)(o.deferralPath)(o.scope)(i)(qe(t))(c)()}}}})}};return new H(u)}}},oT=function(){return function(){return function(n){var e=dS(n);return function(r){return function(t){return function(a){return function(u){var o=e(a)(r)(t),i=function(c){return function(l){return cn(function(s){return function(p){return function(m){return function(){var f=l.ids();return c.raiseId(y(f))(),p(l.makeIIRFilter({id:y(f),parent:c.parent,scope:Vn(c.scope),feedforward:Is(o.feedforward),feedback:Is(o.feedback)}))(),p(l.deferPayload(c.deferralPath)(l.deleteFromCache({id:y(f)})))(),De(m)(f)(c.deferralPath)(c.scope)(l)(qe(u))(s)()}}}})}};return new H(i)}}}}}}},iT=oT()(),cT=function(){return function(){return function(n){return iT(n)(x.value)(x.value)}}},fT=function(n){var e=cS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeRecorder({id:y(m),parent:o.parent,scope:Vn(o.scope),cb:a.cb}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),De(s)(m)(o.deferralPath)(o.scope)(i)(t)(c)()}}}})}};return new H(u)}}},lT=function(n){return function(e){return cn(function(r){return function(t){return function(a){return function(){var o=e.ids();return t(e.makeSpeaker({id:y(o)}))(),De(a)(o)(wn.value)(new tt("toplevel"))(e)(qe(n))(r)()}}}})}},wl=lT,sT=function(n){var e=xm(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeWaveShaper({id:y(m),parent:o.parent,scope:Vn(o.scope),curve:a.curve,oversample:a.oversample}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),De(s)(m)(o.deferralPath)(o.scope)(i)(qe(t))(c)()}}}})}};return new H(u)}}},kn=function(n){return function(e){return function(r){return wt(n)(e)(nr)(r)}}},wt=function(n){var e=hS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeGain({id:y(v),parent:i.parent,scope:Vn(i.scope),gain:u.gain}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({gain:zm(608)(i.scope)(i.deferralPath)(c)(function(d){return c.setGain(function(h){return{id:y(v),gain:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},zm=HS("tmpResolveAU","Ocarina.Control",function(){var n=function(){var o=wu({reflectSymbol:function(){return"unit"}})(x.value);return function(i){return gu(o(i))}}(),e=function(){var o=wu({reflectSymbol:function(){return"sudden"}})(x.value);return function(i){return gu(o(i))}}(),r=function(){var o=wu({reflectSymbol:function(){return"numeric"}})(x.value);return function(i){return gu(o(i))}}(),t=function(){var o=wu({reflectSymbol:function(){return"envelope"}})(x.value);return function(i){return gu(o(i))}}(),a=function(){var o=wu({reflectSymbol:function(){return"cancel"}})(x.value);return function(i){return gu(o(i))}}(),u=function(o){return function(i){return function(c){return function(l){return function(s){return ce({numeric:function(p){return Se(l(r(p)))},envelope:function(p){return Se(l(t(p)))},cancel:function(p){return Se(l(a(p)))},sudden:function(p){return Se(l(e(p)))},unit:function(p){var m=kn(an)(1)([p.u]);return cn(function(v){return function(f){return function(d){return function(){var b=J(_.value)();return d(jS(VS(Rm({parent:_.value,scope:o,deferralPath:i,raiseId:function(O){return GS(Yn(new $(O))(b))}})(c)(m))(QS(pw(function(O,g){return function(){var R=V(b)();if(R instanceof _)return y1(US("Wrapped audio unit failed!"));if(R instanceof $)return g(l(n({i:R.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1668, column 48 - line 1670, column 86): "+[R.constructor.name])}(),Im(w)}))))(v))()}}}})}})(s)}}}}};return u}),_n=zm(1638),vT=kn(an),pT=function(n){var e=wm(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeLoopBuf({id:y(m),parent:o.parent,scope:Vn(o.scope),buffer:a.buffer,playbackRate:a.playbackRate,loopStart:a.loopStart,loopEnd:a.loopEnd,duration:a.duration}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({buffer:function(f){return Se(i.setBuffer({id:y(m),buffer:f}))},playbackRate:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setPlaybackRate(function(d){return{id:y(m),playbackRate:d}}(f))}),loopStart:function(f){return Se(i.setLoopStart({id:y(m),loopStart:f}))},loopEnd:function(f){return Se(i.setLoopEnd({id:y(m),loopEnd:f}))},onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},Wn=function(n){return pT(n)},dT=function(n){var e=fS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makePeriodicOsc({id:y(m),parent:o.parent,scope:Vn(o.scope),frequency:a.frequency,spec:a.spec}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({frequency:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setFrequency(function(d){return{id:y(m),frequency:d}}(f))}),onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))},spec:function(f){return Se(i.setPeriodicOsc({id:y(m),spec:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},bl=function(n){return dT(n)},mT=function(n){var e=ym(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makePlayBuf({id:y(m),parent:o.parent,scope:Vn(o.scope),buffer:a.buffer,playbackRate:a.playbackRate,bufferOffset:a.bufferOffset,duration:a.duration}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({buffer:function(f){return Se(i.setBuffer({id:y(m),buffer:f}))},playbackRate:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setPlaybackRate(function(d){return{id:y(m),playbackRate:d}}(f))}),bufferOffset:function(f){return Se(i.setBufferOffset({id:y(m),bufferOffset:f}))},onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))},duration:function(f){return Se(i.setDuration({id:y(m),duration:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},Yt=function(n){return mT(n)},hT=function(n){var e=iS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeSawtoothOsc({id:y(m),parent:o.parent,scope:Vn(o.scope),frequency:a.frequency}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({frequency:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setFrequency(function(d){return{id:y(m),frequency:d}}(f))}),onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},xT=function(n){return hT(n)},yT=function(n){var e=oS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeSinOsc({id:y(m),parent:o.parent,scope:Vn(o.scope),frequency:a.frequency}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({frequency:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setFrequency(function(d){return{id:y(m),frequency:d}}(f))}),onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},rc=function(n){return yT(n)},gT=function(n){var e=rc(n);return function(r){return e(r)(nr)}},wT=function(n){var e=uS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeSquareOsc({id:y(m),parent:o.parent,scope:Vn(o.scope),frequency:a.frequency}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({frequency:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setFrequency(function(d){return{id:y(m),frequency:d}}(f))}),onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},$l=function(n){return wT(n)},bT=function(n){var e=$l(n);return function(r){return e(r)(nr)}},$T=function(n){var e=tS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeTriangleOsc({id:y(m),parent:o.parent,scope:Vn(o.scope),frequency:a.frequency}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({frequency:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setFrequency(function(d){return{id:y(m),frequency:d}}(f))}),onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},Lm=function(n){return $T(n)},_T=function(n){var e=Om(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeAllpass({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,q:u.q}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),q:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setQ(function(h){return{id:y(v),q:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},Mm=function(n){var e=_T(n);return function(r){return function(t){return e(r)(nr)(t)}}},Wm=function(n){var e=_m(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeBandpass({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,q:u.q}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),q:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setQ(function(h){return{id:y(v),q:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},_o=function(n){var e=Wm(n);return function(r){return function(t){return e(r)(nr)(t)}}},tc=function(n){var e=$m(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeDelay({id:y(v),parent:i.parent,scope:Vn(i.scope),delayTime:u.delayTime,maxDelayTime:u.maxDelayTime}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({delayTime:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setDelay(function(h){return{id:y(v),delayTime:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},ac=function(n){var e=tc(n);return function(r){return function(t){return e(r)(nr)(t)}}},OT=function(n){var e=xS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeDynamicsCompressor({id:y(v),parent:i.parent,scope:Vn(i.scope),threshold:u.threshold,ratio:u.ratio,knee:u.knee,attack:u.attack,release:u.release}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({threshold:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setThreshold(function(h){return{id:y(v),threshold:h}}(d))}),ratio:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setRatio(function(h){return{id:y(v),ratio:h}}(d))}),knee:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setKnee(function(h){return{id:y(v),knee:h}}(d))}),attack:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setAttack(function(h){return{id:y(v),attack:h}}(d))}),release:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setRelease(function(h){return{id:y(v),release:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},kT=function(n){var e=OT(n);return function(r){return e(r)(nr)}},ST=function(){return function(n){return function(e){return YS({doLogic:pf,deferPayload:function(r){return r.deferPayload},forcePayload:function(r){return r.forcePayload},ids:function(r){return function(t){return t.ids}(hi(r))},disconnectElement:function(r){return function(t){return r.disconnectXFromY({from:t.id,to:t.parent})}},toElt:function(r){return r}})({fromEltO1:Uu,fromEltO2:Uu,toElt:Uu,wrapElt:function(r){return vT(1)([r])},giveNewParent:function(r){return function(t){return function(a){return function(u){return r.connectXToY({from:t.id,to:t.parent})}}}},deleteFromCache:function(r){return function(t){return t.deleteFromCache}(hi(r))}})(n)(qm(KS(function(r){return r(w)}))(Uu(e)))}}},TT=ST(),Kt=function(n){return function(e){return TT(ib(n))(qm(XS(x.value))(e))}},Um=function(n){var e=bm(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeHighpass({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,q:u.q}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),q:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setQ(function(h){return{id:y(v),q:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},_l=function(n){var e=Um(n);return function(r){return function(t){return e(r)(nr)(t)}}},PT=function(n){var e=mS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeHighshelf({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,gain:u.gain}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),gain:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setGain(function(h){return{id:y(v),gain:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},AT=function(n){var e=PT(n);return function(r){return function(t){return e(r)(nr)(t)}}},ET=function(n){var e=gm(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeLowpass({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,q:u.q}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),q:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setQ(function(h){return{id:y(v),q:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},Ol=function(n){var e=ET(n);return function(r){return function(t){return e(r)(nr)(t)}}},IT=function(n){var e=pS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeLowshelf({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,gain:u.gain}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),gain:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setGain(function(h){return{id:y(v),gain:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},qT=function(n){var e=IT(n);return function(r){return function(t){return e(r)(nr)(t)}}},FT=function(n){var e=sS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeNotch({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,q:u.q}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),q:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setQ(function(h){return{id:y(v),q:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},CT=function(n){var e=FT(n);return function(r){return function(t){return e(r)(nr)(t)}}},BT=function(n){var e=aS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makeStereoPanner({id:y(v),parent:i.parent,scope:Vn(i.scope),pan:u.pan}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({pan:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setPan(function(h){return{id:y(v),pan:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},DT=function(n){var e=BT(n);return function(r){return e(r)(nr)}},RT=function(n){var e=lS(n);return function(r){return function(t){return function(a){var u=e(r),o=function(i){return function(c){return cn(function(l){return function(s){return function(p){return function(){var v=c.ids();return i.raiseId(y(v))(),s(c.makePeaking({id:y(v),parent:i.parent,scope:Vn(i.scope),frequency:u.frequency,q:u.q,gain:u.gain}))(),s(c.deferPayload(i.deferralPath)(c.deleteFromCache({id:y(v)})))(),De(p)(v)(i.deferralPath)(i.scope)(c)(qe(a))(l)(),de(p)(l)(Oe(pe(function(f){return ce({frequency:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setFrequency(function(h){return{id:y(v),frequency:h}}(d))}),q:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setQ(function(h){return{id:y(v),q:h}}(d))}),gain:_n(i.scope)(i.deferralPath)(c)(function(d){return c.setGain(function(h){return{id:y(v),gain:h}}(d))})})(f)})(t)))()}}}})}};return new H(o)}}}},NT=function(n){var e=RT(n);return function(r){return function(t){return e(r)(nr)(t)}}},zT=function(n){var e=gS(n);return function(r){return function(t){var a=e(r),u=function(o){return function(i){return cn(function(c){return function(l){return function(s){return function(){var m=i.ids();return o.raiseId(y(m))(),l(i.makeConstant({id:y(m),parent:o.parent,scope:Vn(o.scope),offset:a.offset}))(),l(i.deferPayload(o.deferralPath)(i.deleteFromCache({id:y(m)})))(),de(s)(c)(Oe(pe(function(v){return ce({offset:_n(o.scope)(o.deferralPath)(i)(function(f){return i.setOffset(function(d){return{id:y(m),offset:d}}(f))}),onOff:function(f){return Se(i.setOnOff({id:y(m),onOff:f}))}})(v)})(t)))()}}}})}};return new H(u)}}},Hm=function(n){return zT(n)};function LT(){window.scrollTo(0,0)}var MT=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),qo=function(){function n(){}return n.value=new n,n}(),xi=function(){function n(){}return n.value=new n,n}(),Vc=function(){function n(){}return n.value=new n,n}(),yi=function(){function n(){}return n.value=new n,n}(),Gc=function(){function n(){}return n.value=new n,n}(),Qc=function(){function n(){}return n.value=new n,n}(),WT=function(){function n(){}return n.value=new n,n}(),jm=function(){function n(){}return n.value=new n,n}(),Vm=function(){function n(){}return n.value=new n,n}(),Yc=function(){function n(){}return n.value=new n,n}(),Kc=function(){function n(){}return n.value=new n,n}(),UT=function(){function n(){}return n.value=new n,n}(),bu=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),qs=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),HT="numeric",jT="sudden",VT="unit",GT="cancel",QT="step",YT="linear",KT="exponential",XT="envelope",Gm=function(n,e,r,t){if(r.type===jT)n.value=r.value.n;else if(r.type===VT)e.id&&e3(e.id,t),t.units[r.value.i].main.connect(n),e.id=r.value.i;else if(r.type===HT)n[r.value.t.type===QT?"setValueAtTime":r.value.t.type===YT?"linearRampToValueAtTime":r.value.t.type===KT?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,r.value.o);else if(r.type===GT)r.value.hold?n.cancelAndHoldAtTime(r.value.o):n.cancelScheduledValues(r.value.o);else if(r.type===XT){const a=r.value.o;n.cancelScheduledValues(Math.max(0,a)),n.setValueCurveAtTime(r.value.p,a,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},JT=function(n,e,r,t,a){return t[r]||(t[r]={}),Gm(e.parameters.get(r),t[r],a,n)},Or=function(n,e,r,t,a){return t[r]||(t[r]={}),Gm(e[r],t[r],a,n)},Bn=function(n,e,r,t){const a=n("@fan@")(u=>u)(r);t.scopes[a]||(t.scopes[a]=[]),t.scopes[a].push(e),t.units[e].scope=a},Dn=function(n,e){e.toConnect[n]&&(e.toConnect[n].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[n])},Rn=function(n,e,r,t){n()(a=>Qm(e,a,t))(r)},Qm=function(n,e,r){var t=function(){r.units[n].audioOutgoing.push(e),r.units[n].pendingOn||(r.units[n].main.connect(r.units[e].main),r.units[e].se&&r.units[n].main.connect(r.units[e].se))};if(!r.units[n]){r.toConnect[n]||(r.toConnect[n]=[]);var a={f:t};e!==n&&!r.units[e]&&(a.w=e),r.toConnect[n].push(a);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var a={f:t};e!==n&&!r.units[n]&&(a.w=n),r.toConnect[e].push(a);return}t()};function ZT(n){return function(e){return function(){delete e.units[n.id]}}}function n3(n){return function(e){return function(){Qm(n.from,n.to,e)}}}var e3=function(n,e){if(e.units[n].scope==="@fan@")return;const r=e.units[n].scope;e.scopes[r].forEach(t=>{delete e.units[t]}),delete e.scopes[r]};function r3(n){return function(e){return function(){var r=n.from,t=n.to;if(!e.units[r]||(e.units[r].audioOutgoing=e.units[r].audioOutgoing.filter(function(u){return u!==t}),e.units[r].main.disconnect(e.units[t].main),e.units[t].se&&e.units[r].main.disconnect(e.units[t].se),e.units[r].scope==="@fan@"))return;const a=e.units[r].scope;e.scopes[a].forEach(u=>{delete e.units[u]}),delete e.scopes[a]}}}const t3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"allpass",Q:e.q,frequency:e.frequency})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},a3=n=>e=>r=>()=>{var t=e.id,a=e.cb,u=new AnalyserNode(r.context,e),o=a(u)();r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:o,main:r.context.createGain(),se:u},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},u3=n=>e=>r=>()=>{var t=e.id,a=e.options;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(r.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},o3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"bandpass",Q:e.q,frequency:e.frequency})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},i3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){return new ConstantSourceNode(o,i)},u={offset:e.offset};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},c3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(r.context,{buffer:e.buffer})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},f3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(r.context,{delayTime:e.delayTime,maxDelayTime:e.maxDelayTime})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},l3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(r.context,{knee:e.knee,ratio:e.ratio,threshold:e.threshold,attack:e.attack,release:e.release})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},s3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(r.context,{gain:e.gain})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},v3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"highpass",Q:e.q,frequency:e.frequency})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},p3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"highshelf",frequency:e.frequency,gain:e.gain})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},d3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(r.context,{feedforward:e.feedforward,feedback:e.feedback})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},m3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){return new AudioBufferSourceNode(o,i)},u={loop:!0,buffer:e.buffer,loopStart:e.loopStart,loopEnd:e.loopEnd,playbackRate:e.playbackRate};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},h3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"lowpass",Q:e.q,frequency:e.frequency})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},x3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"lowshelf",frequency:e.frequency,gain:e.gain})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},y3=n=>e=>r=>()=>{var t=e.id,a=e.element,u=function(){var o=r.context.createMediaElementSource(a);return o};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},g3=n=>e=>r=>()=>{var t=e.id;r.units[e.id]={main:r.context.createMediaStreamSource(e.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},w3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"notch",frequency:e.frequency,Q:e.q})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},b3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(r.context,{type:"peaking",frequency:e.frequency,Q:e.q,gain:e.gain})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},$3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){var c={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:cP(r.context)(i.spec.value.real)(i.spec.value.img)()},l=new OscillatorNode(o,c);return l},u={frequency:e.frequency,type:"custom",spec:e.spec};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},_3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){var c={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(o,c)},u={loop:!1,buffer:e.buffer,playbackRate:e.playbackRate,bufferOffset:e.bufferOffset,duration:n(void 0)(o=>o)(e.duration)};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},O3=n=>e=>r=>()=>{var t=e.id,a=e.cb,u=r.context.createMediaStreamDestination(),o=new MediaRecorder(u.stream);a(o)(),o.start(),r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:o,main:r.context.createGain(),se:u},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},k3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:e.frequency,type:"sawtooth"};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},S3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:e.frequency,type:"sine"};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},T3=n=>e=>()=>{e.units[n.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:e.context.createGain(),se:e.context.destination}},P3=n=>e=>r=>()=>{var t=e.id;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(r.context,{pan:e.pan})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},A3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:e.frequency,type:"square"};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},E3=n=>e=>r=>()=>{var t=e.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:e.frequency,type:"triangle"};r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(r.context,u)},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)},I3=n=>e=>r=>()=>{var t=e.id,a=e.curve,u=e.oversample;r.units[t]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(r.context,{curve:a,oversample:u.type})},Bn(n,t,e.scope,r),Dn(t,r),Rn(n,t,e.parent,r)};function q3(n){return function(e){return function(){var r=n.id,t=n.cb;e.units[r].analyserOrig!==t&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=t(e.units[r].se)(),e.units[r].analyserOrig=t)}}}function F3(n){return function(e){return function(){var r=n.cb,t=n.id;if(e.units[t].recorderOrig!==r){e.units[t].recorder&&e.units[t].recorder.stop();var a=r;e.units[t].recorderOrig=r;var u=new MediaRecorder(e.units[t].se);a(u)(),u.start()}}}}function C3(n){return function(e){return function(){var r=n.id,t=n.curve;e.units[r].main.curve=t}}}function B3(n){return function(e){return function(){var r=n.id,t=n.paramName,a=n.paramValue;JT(e,e.units[r].main,t,e.units[r].controllers,a)}}}const kr=function(n,e,r){e.resume&&n.value.n!==void 0&&(e.resume[r]=n.value.n)};function D3(n){return function(e){return function(){var r=n.id,t=n.gain;Or(e,e.units[r].main,"gain",e.units[r].controllers,t),kr(t,e.units[r],"gain")}}}function R3(n){return function(e){return function(){var r=n.id,t=n.q;Or(e,e.units[r].main,"Q",e.units[r].controllers,t),kr(t,e.units[r],"Q")}}}function N3(n){return function(e){return function(){var r=n.id,t=n.buffer;e.units[r].resume&&(e.units[r].resume.buffer=t)}}}function z3(n){return function(e){return function(){var r=n.id,t=n.buffer;e.units[r].main.buffer=t}}}function L3(n){return function(e){return function(){var r=n.id,t=n.spec;e.units[r].resume&&(e.units[r].resume.spec=t)}}}function M3(n){return function(e){return function(){var r=n.id,t=n.pan;Or(e,e.units[r].main,"pan",e.units[r].controllers,t),kr(t,e.units[r],"pan")}}}function W3(n){return function(e){return function(){var r=n.id,t=n.threshold;Or(e,e.units[r].main,"threshold",e.units[r].controllers,t),kr(t,e.units[r],"threshold")}}}function U3(n){return function(e){return function(){var r=n.id,t=n.loopStart;e.units[r].main.loopStart=t,e.units[r].resume.loopStart=t}}}function H3(n){return function(e){return function(){var r=n.id,t=n.loopEnd;e.units[r].main.loopEnd=t,e.units[r].resume.loopEnd=t}}}function j3(n){return function(e){return function(){var r=n.id,t=n.bufferOffset;e.units[r].resume.bufferOffset=t}}}function V3(n){return function(e){return function(r){return function(){var t=e.id,a=e.duration;r.units[t].duration=n(void 0)(u=>u)(a)}}}}function G3(n){return function(e){return function(){var r=n.id,t=n.release;Or(e,e.units[r].main,"release",e.units[r].controllers,t),kr(t,e.units[r],"release")}}}function Q3(n){return function(e){return function(){var r=n.id,t=n.offset;Or(e,e.units[r].main,"offset",e.units[r].controllers,t),kr(t,e.units[r],"offset")}}}function Y3(n){return function(e){return function(){var r=n.id,t=n.ratio;Or(e,e.units[r].main,"ratio",e.units[r].controllers,t),kr(t,e.units[r],"ratio")}}}function K3(n){return function(e){return function(){var r=n.id,t=n.attack;Or(e,e.units[r].main,"attack",e.units[r].controllers,t),kr(t,e.units[r],"attack")}}}function X3(n){return function(e){return function(){var r=n.id,t=n.knee;Or(e,e.units[r].main,"knee",e.units[r].controllers,t),kr(t,e.units[r],"knee")}}}function J3(n){return function(e){return function(){var r=n.id,t=n.delayTime;Or(e,e.units[r].main,"delayTime",e.units[r].controllers,t),kr(t,e.units[r],"delayTime")}}}function Z3(n){return function(e){return function(){var r=n.id,t=n.playbackRate;Or(e,e.units[r].main,"playbackRate",e.units[r].controllers,t),kr(t,e.units[r],"playbackRate")}}}function nP(n){return function(e){return function(){var r=n.id,t=n.frequency;Or(e,e.units[r].main,"frequency",e.units[r].controllers,t),kr(t,e.units[r],"frequency")}}}function eP(n){return function(e){return function(){var r=n.id,t=n.onOff;t.x.type==="on"?rP(r)(t)(e)():t.x.type==="off"&&tP(r)(t)(e)()}}}var rP=function(n){return function(e){return function(r){return function(){if(!r.units[n].onOff){r.units[n].pendingOn=!1,r.units[n].onOff=!0,r.units[n].main=r.units[n].createClosure(r.context,r.units[n].resume);for(var t=0;t<r.units[n].audioOutgoing.length;t++){var a=r.units[n].audioOutgoing[t];r.units[n].main.connect(r.units[a].main),r.units[a].se&&r.units[n].main.connect(r.units[a].se)}r.units[n].resume&&r.units[n].resume.bufferOffset?typeof r.units[n].resume.duration=="number"?r.units[n].main.start(r.deprecatedWriteHead+e.o,r.units[n].resume.bufferOffset,r.units[n].resume.duration):r.units[n].main.start(r.deprecatedWriteHead+e.o,r.units[n].resume.bufferOffset):r.units[n].resume&&r.units[n].resume.loopStart?r.units[n].main.start(r.deprecatedWriteHead+e.o,r.units[n].resume.loopStart):r.units[n].main.start(r.deprecatedWriteHead+e.o)}}}}},tP=function(n){return function(e){return function(r){return function(){if(r.units[n].onOff){r.units[n].onOff=!1;var t=r.units[n].main;t.addEventListener("ended",()=>{t.disconnect()}),t.stop(r.deprecatedWriteHead+e.o)}}}}};function aP(n){for(var e=new Float32Array(n.length),r=0;r<n.length;r++)e[r]=n[r];return e}function Fs(n){return function(){n.stop()}}function uP(n){return function(e){return function(r){return function(){var t=[];r.ondataavailable=function(a){t.push(a.data)},r.onstop=function(){var a=new Blob(t,{type:n});e(a)(),t=null}}}}}function oP(n){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:n,video:e})}}}function Ym(n){return function(){var e=new Uint8Array(n.frequencyBinCount);return n.getByteFrequencyData(e),e}}function iP(n){return function(){var e=n.createConstantSource();return e.offset.value=0,e.connect(n.destination),e.start(),function(){e.stop(),e.disconnect(n.destination)}}}var cP=function(n){return function(e){return function(r){return function(){for(var t=new Float32Array(e.length),a=new Float32Array(r.length),u=0;u<e.length;u++)t[u]=e[u];for(var u=0;u<r.length;u++)a[u]=r[u];return n.createPeriodicWave(t,a,{disableNormalization:!0})}}}};function kl(n){return function(){return{context:n,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function fP(n){return function(){n.close()}}function lP(n){return function(){return fetch(n).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function sP(n){return function(e){return function(){return n.decodeAudioData(e)}}}function vP(){return new(window.AudioContext||window.webkitAudioContext)}function pP(n){return function(){return n.state}}function Xc(n){return function(){return n.currentTime}}function dP(n){return function(e){return function(r){return function(){n.then(r,e)}}}}var mP=Yh(Ee),hP=Xe(q1),xP=An(Tn),yP=ve(f1(hg)(ga)),gP=Kd(ga),wP=k(nl(bf)),bP=IO(ga),$P=Zn(Zr),_P=br(Ge),OP=function(n){return function(e){return Zf(function(r){return mP(hP)(dP(e)(function(t){return r(Qn.create(n(t)))()})(function(t){return r(Ln.create(t))()}))})}},kP=function(n){return Ha(function(e){return mi("Promise failed, couldn't extract JS Error or String")})(xP)(TO(yP(gP("Error")(n))(wP(mi)(bP(n)))))},SP=OP(kP),Jc=function(n){return $P(_P(n))(SP)};function TP(n){return function(){return URL.createObjectURL(n)}}var PP=bi(Nt),AP=yr(lo),Sl=Qp(Ka),Cs=Wp(Tn),EP=Zp(Sl)(Ip()(fi({reflectSymbol:function(){return"instructions"}})(Cs)()(fi({reflectSymbol:function(){return"newMap"}})(Cs)()(Ap)))),IP=Si(Sl),qP=ff(sf),Bs=ba(),FP=Dt(co),CP=Cf(Sl),BP=se(ma),DP=Pe(rr),RP=Zn(Zr),NP=Ya(gn),zP=k(go),Ds=S(Op),LP=function(n){return function(e){return function(r){return Kn(uP(n))(r)(function(){var t=PP(e);return function(a){return t(TP(a))}}())}}},MP=function(n){var e=Pe(n);return function(r){var t=r.Monad0(),a=t.Bind1(),u=Zn(a),o=Ie(r),i=qP(a),c=vr(t.Applicative0())(he);return function(l){return function(s){return function(p){var m=function(v){return u(o(V(l)))(function(f){var d=function(g){return g instanceof E&&g.value1 instanceof wn?new E(g.value0+1|0,wn.value):g instanceof E?new E(g.value0,d(g.value1)):g},h=new $(d(p)),b=new $(p),O=Kn(EP(b)(h))(f)(function(g){return function(G){return{newMap:IP(g),instructions:ki(G)}}});return i(e(o(ie(Bs(O.newMap))(l))))(function(){return c(FP(Bs(O.instructions)([])))(s)})})};return m}}}}},WP=MP(Ee)(Je),UP=function(n){var e=Pe(n);return function(r){var t=Ie(r);return function(a){return function(u){return function(o){return function(i){return e(t(ie(Kn(CP)(u)(function(c){if(c instanceof _)return new $([o]);if(c instanceof $)return new $(BP(c.value0)([o]));throw new Error("Failed pattern match at Ocarina.Interpret (line 427, column 24 - line 429, column 36): "+[c.constructor.name])}))(a)))}}}}}},HP=UP(Ee)(Je),Tl=function(n){return function(e){return function(r){return{ids:function(){var a=V(n)();return DP(ie(AP(1))(n))(),a},deleteFromCache:ZT,deferPayload:HP(e),forcePayload:WP(e)(r),disconnectXFromY:r3,connectXToY:n3,makeAllpass:t3(pn),makeAnalyser:a3(pn),makeAudioWorkletNode:u3(pn),makeBandpass:o3(pn),makeConstant:i3(pn),makeConvolver:c3(pn),makeDelay:f3(pn),makeDynamicsCompressor:l3(pn),makeGain:s3(pn),makeHighpass:v3(pn),makeHighshelf:p3(pn),makeIIRFilter:d3(pn),makeLoopBuf:m3(pn),makeLowpass:h3(pn),makeLowshelf:x3(pn),makeMediaElement:y3(pn),makeMicrophone:g3(pn),makeNotch:w3(pn),makePeaking:b3(pn),makePeriodicOsc:$3(pn),makePlayBuf:_3(pn),makeRecorder:O3(pn),makeSawtoothOsc:k3(pn),makeSinOsc:S3(pn),makeSpeaker:T3,makeSquareOsc:A3(pn),makeStereoPanner:P3(pn),makeTriangleOsc:E3(pn),makeWaveShaper:I3(pn),setAnalyserNodeCb:q3,setMediaRecorderCb:F3,setWaveShaperCurve:C3,setAudioWorkletParameter:B3,setBuffer:N3,setConvolverBuffer:z3,setDuration:V3(pn),setPeriodicOsc:L3,setOnOff:eP,setBufferOffset:j3,setLoopStart:U3,setLoopEnd:H3,setRatio:Y3,setOffset:Q3,setAttack:K3,setGain:D3,setQ:R3,setPan:M3,setThreshold:W3,setRelease:G3,setKnee:X3,setDelay:J3,setPlaybackRate:Z3,setFrequency:nP}}}},rn=function(n){return function(e){return RP(Jc(lP(e)))(function(){var r=sP(n);return function(t){return Jc(r(t))}}())}},Pl=function(n){var e=br(n);return function(r){return e(pP(r))}},jP=Pl(ge),bt=function(n){return br(n)(vP)},hu=function(n){var e=br(n);return function(r){return e(iP(r))}},nt=function(n){var e=br(n);return function(r){return e(function(){var a=jP(r)();return NP(a!=="closed")(fP(r))()})}},VP=qr,GP=qr,Km=function(n){return function(e){return zP(function(r){return{microphone:function(){return n?Ds(VP(r)):_.value}(),camera:function(){return e?Ds(GP(r)):_.value}()}})(Jc(oP(n)(e)))}},QP=yo(st),YP=ve(F1),Rs=uu(st),ti=Zn(Zr),Xm=br(Ge),Jm=Ve(He),Zm=ve(Jm),nh=jn(Z),eh=S(nh),Ua=k(Hn(U)),Hu=Jn(ue),vc=S(gn),KP=bt(Ge),XP=hu(Ge),JP=nt(ge),ZP=ne(Xr(Z)),qt=function(){function n(){}return n.value=new n,n}(),Ft=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Ar=function(){function n(){}return n.value=new n,n}(),Rr=LT,Oo=function(n){return QP(YP(Rs(ti(S1(n))(Xm)))(Rs(T1(mi("We navigated away from the page"))(n))))},Al=function(n){var e=ve(n);return function(r){var t=S(r);return function(a){return function(u){return e(t(Ar.value))(u)}}}},rh=Al(Jm)(nh),Xt=function(n){return function(e){return me(le)(Zm(eh(e))(Ua(function(r){return Hu(r.value0)(e)})(n)))}},El=function(n){return function(e){return function(r){return function(t){return function(a){return function(u){return me(le)(Ua(function(o){if(o.value0 instanceof qt)return vc(w);if(o.value0 instanceof Ft)return Hu(Hu(o.value0.value0)(n(vc(w))))(e(Ar.value));if(o.value0 instanceof Ar)return function(){o.value1(),e(qt.value)();var c=ou(ti(KP)(function(l){return ti(XP(l))(function(s){return ti(r(l))(function(p){return Xm(function(){var v=t(l)(p)(),f=Hu(Hu(v)(s))(JP(l));return e(new Ft(f))(),f})})})}))();return n(function(){return e(Ar.value)(),iu(Oo(c))()})(),w};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 59, column 21 - line 77, column 18): "+[o.value0.constructor.name])})(ZP(Ua(q.create)(u))(Zm(eh(vc(w)))(Ua(function(o){return o.value0})(a)))))}}}}}},Jt=function(n){return function(e){return function(r){return function(){return n(r)(),e(new MT(r))()}}}},th=function(n){return function(e){return function(r){return function(t){return function(a){return vn(qn)(function(u){var o=rh(e)(u.value1);return Jf([M("cursor: pointer;"),El(r)(u.value0)(t)(a)(e)(o)])([wr(Ua(function(i){if(i instanceof Ar)return n;if(i instanceof qt)return"⏳";if(i instanceof Ft)return"🛑";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 127, column 15 - line 130, column 33): "+[i.constructor.name])})(o))])})}}}}},dn=function(n){return function(e){return function(r){return function(t){return vn(qn)(function(a){var u=rh(n)(a.value1);return Jr([El(e)(a.value0)(r)(t)(n)(u)])([wr(Ua(function(o){if(o instanceof Ar)return"Turn on";if(o instanceof qt)return"Loading...";if(o instanceof Ft)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 100, column 15 - line 103, column 40): "+[o.constructor.name])})(u))])})}}}},$u=Ie(Je),nA=Be(je()),eA=An(Tn),rA=bt(ge),tA=k(Ee),aA=Jn(ue),uA=nt(ge),Q=function(n){return function(e){return function(){var t=kl(n)(),a=$u(J(0))(),u=$u(J(Ja))(),o=function(l){return l(t)},i=$u(_a)(),c=$u(sr(nA(wl(e)(Tl(a)(u)(o)))(i.event))(o))();return i.push(eA)(),$u(c)}}},ah=function(n){return function(){var r=rA();return tA(function(t){return aA(t)(uA(r))})(Q(r)(n))()}},oA=Mn({reflectType:function(){return`<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.2
      [ b
      , allpass_ 700.0
          [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
          , allpass_ 1110.0
              [ b
              , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
              ]
          ]
      ]
  ]
</code></pre>

  @allpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),iA=Wn(ee),cA=nn(),fA=kn(an),Ns=Mm(WS),zs=Mm(Am(On($e()(j(j(be)(eS)()()()({reflectSymbol:function(){return"q"}}))(hm)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),lA=function(){return x.value}(),sA=function(n){return function(e){return function(r){return oA(x.value)(lA)({allpass:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([Kt(iA(a)(cA))(function(u){return fA(.2)([u,Ns(700)([zs({frequency:990,q:20})([u]),Ns(1110)([u,zs({frequency:2010,q:30})([u])])])])})])}})})}}};function vA(n){for(var e=n.length,r=new Array(e),t=0;t<e;t++)r[t]=n[t];return r}var uh=function(n){return function(e){return function(){return vA(e)}}},_u=pt,Ou=pt,ku=pt,ur=pt,or=pt,ir=pt,cr=pt,fr=pt;function pA(n){return n|0}var oh=Qd(dt),Zc=_e(),nf=_e(),ef=_e(),rf=_e(),tf=_e(),dA=$e(),mA=j(j(be)(Cm)()()()({reflectSymbol:function(){return"fftSize"}})),hA={reflectSymbol:function(){return"cb"}},xA=we()(),yA=nn(),ih=k(Hn(U)),gA=Tp(Sp),wA=xn({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(z()(I)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})),ch=je(),bA=e0(ru(U)(eu)(ch)),Ls=An(Tn),$A=Al(Ve(He))(jn(Z)),ca=Ie(Je),_A=Be(ch),OA=vr(gn)(tr),kA=uh(),pc=yr(lo),SA=po(hO)(gn),dc=k(Ee),TA=_e(),PA=_e(),AA=_e(),EA=Pl(ge),IA=Ya(gn),qA=nt(ge),FA=Qd($r),CA="background-color: rgb(150,30,10);",BA="background-color: rgb(130,60,10);",DA="background-color: rgb(80,90,10);",RA="background-color: rgb(10,130,10);",NA="background-color: rgb(10,100,0);",zA=oh(function(n){return Zc(CA)(nf(BA)(ef(DA)(rf(RA)(tf(NA)(Bt)))))}),LA=function(n){var e=Nm(Bm(On(dA(mA(n)()()()(hA)))(xA)));return function(r){var t=Wn(r);return function(a){return function(u){return e({cb:u,fftSize:Jd.value})([t(a)(yA)])}}}},MA=LA(Fm)(ee),WA=function(){return x.value}(),en="background-color: rgb(255,255,255,0.0);",ko=function(n){var e=Os(n)();return function(r){return function(t){var a=Os(t)();return function(u){return function(o){return function(i){return function(c){return function(l){return function(s){return eo(ih(function(p){var m=e(a(p)(c))(l);return m?e(a(zA)(c))(l):en})(s))}}}}}}}}},Zt=ko(Ht)(gt),UA=Zt(Ht)(Qr)(gt)(Qr),HA=Zt(Ut)(Gr)(gt)(Gr),jA=Zt(Wt)(Vr)(gt)(Vr),VA=Zt(Mt)(jr)(gt)(jr),GA=Zt(Lt)(Hr)(gt)(Hr),QA=Zt($r)(Ur)(gt)(Ur),YA=Zt(lu)(Wr)(gt)(Wr),KA=Zt(fu)(Mr)(gt)(Mr),na=ko(Ut)(yt),XA=na(Ht)(Qr)(yt)(Qr),JA=na(Ut)(Gr)(yt)(Gr),ZA=na(Wt)(Vr)(yt)(Vr),nE=na(Mt)(jr)(yt)(jr),eE=na(Lt)(Hr)(yt)(Hr),rE=na($r)(Ur)(yt)(Ur),tE=na(lu)(Wr)(yt)(Wr),aE=na(fu)(Mr)(yt)(Mr),ea=ko(Wt)(xt),uE=ea(Ht)(Qr)(xt)(Qr),oE=ea(Ut)(Gr)(xt)(Gr),iE=ea(Wt)(Vr)(xt)(Vr),cE=ea(Mt)(jr)(xt)(jr),fE=ea(Lt)(Hr)(xt)(Hr),lE=ea($r)(Ur)(xt)(Ur),sE=ea(lu)(Wr)(xt)(Wr),vE=ea(fu)(Mr)(xt)(Mr),ra=ko(Mt)(ht),pE=ra(Ht)(Qr)(ht)(Qr),dE=ra(Ut)(Gr)(ht)(Gr),mE=ra(Wt)(Vr)(ht)(Vr),hE=ra(Mt)(jr)(ht)(jr),xE=ra(Lt)(Hr)(ht)(Hr),yE=ra($r)(Ur)(ht)(Ur),gE=ra(lu)(Wr)(ht)(Wr),wE=ra(fu)(Mr)(ht)(Mr),ta=ko(Lt)(mt),bE=ta(Ht)(Qr)(mt)(Qr),$E=ta(Ut)(Gr)(mt)(Gr),_E=ta(Wt)(Vr)(mt)(Vr),OE=ta(Mt)(jr)(mt)(jr),kE=ta(Lt)(Hr)(mt)(Hr),SE=ta($r)(Ur)(mt)(Ur),TE=ta(lu)(Wr)(mt)(Wr),PE=ta(fu)(Mr)(mt)(Mr),AE=function(){return 15/40}(),EE=function(){return 10/40}(),IE=function(){return 7/40}(),qE=function(){return 3/40}(),FE=function(){return 1/40}(),CE=function(n){return function(e){return function(r){return wA(WA)({analyser:vn(qn)(function(t){var a=bA(Ls)(t.value1),u=$A(r)(function(i){return i.right}(a)),o=function(i){return i.left}(a);return zn([Jr([M("cursor: pointer;"),El(n)(function(i){return t.value0(Ln.create(i))})(function(i){return rn(i)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(i){return function(c){return function(){var s=Qe(_.value)(),p=ca(_a)(),m=kl(i)(),v=ca(J(0))(),f=ca(J(Ja))(),d=function(G){return G(m)},h=wl([MA(c)(function(G){return function(){return ja(new $(G))(s)(),ja(_.value)(s)}})])(Tl(v)(f)(d)),b=Fi(),O=ca(sr(_A(h)(p.event))(d))(),g=ca(sr(b.event)(function(G){return function(){var Un=va(s)();return OA(Un)(function(Nn){return function(){var A=Ym(Nn)(),yn=kA(A)(),Sn=Qe(0)(),mn=Qe(0)(),bn=Qe(0)(),Me=Qe(0)(),We=Qe(0)(),Y=Qe(0)(),Gn=Qe(0)(),Re=Qe(0)(),_t=Qe(0)(),Ta=Qe(0)(),P=function(un){return un<32?Sn:un<64?mn:un<96?bn:un<128?Me:un<168?We:un<160?Y:un<224?Gn:Re};$i(yn)(function(un){var on=pA(un);return function(){var T=va(Ta)();return cc(pc(on))(_t)(),cc(pc(on))(P(T))(),cc(pc(1))(Ta)()}})();var fn=SA(function(un){return function(){var F=dc(ae)(va(un))(),T=dc(gA(F))(dc(ae)(va(_t)))();return Zc(T>AE)(nf(T>EE)(ef(T>IE)(rf(T>qE)(tf(T>FE)(Bt)))))}})(TA(Sn)(PA(mn)(AA(bn)(Zc(Me)(nf(We)(ef(Y)(rf(Gn)(tf(Re)(Bt)))))))))();return t.value0(new Qn(fn))()}})()}}))();return p.push(Ls)(),function(){return ca(O)(),ca(g)(),b.unsubscribe(),function(){var Un=EA(i)();return IA(Un!=="closed")(qA(i))()}(),t.value0(new Qn(oh(Fn(FA(Fn(!1))))))()}}}})(r)(u)])([wr(ih(function(i){if(i instanceof Ar)return"Turn on";if(i instanceof qt)return"Loading...";if(i instanceof Ft)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 171, column 23 - line 174, column 48): "+[i.constructor.name])})(u))]),X([M("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")])([X([M(en),UA(fr)(fr)(o)])([]),X([M(en),HA(cr)(fr)(o)])([]),X([M(en),jA(ir)(fr)(o)])([]),X([M(en),VA(or)(fr)(o)])([]),X([M(en),GA(ur)(fr)(o)])([]),X([M(en),QA(ku)(fr)(o)])([]),X([M(en),YA(Ou)(fr)(o)])([]),X([M(en),KA(_u)(fr)(o)])([]),X([M(en),XA(fr)(cr)(o)])([]),X([M(en),JA(cr)(cr)(o)])([]),X([M(en),ZA(ir)(cr)(o)])([]),X([M(en),nE(or)(cr)(o)])([]),X([M(en),eE(ur)(cr)(o)])([]),X([M(en),rE(ku)(cr)(o)])([]),X([M(en),tE(Ou)(cr)(o)])([]),X([M(en),aE(_u)(cr)(o)])([]),X([M(en),uE(fr)(ir)(o)])([]),X([M(en),oE(cr)(ir)(o)])([]),X([M(en),iE(ir)(ir)(o)])([]),X([M(en),cE(or)(ir)(o)])([]),X([M(en),fE(ur)(ir)(o)])([]),X([M(en),lE(ku)(ir)(o)])([]),X([M(en),sE(Ou)(ir)(o)])([]),X([M(en),vE(_u)(ir)(o)])([]),X([M(en),pE(fr)(or)(o)])([]),X([M(en),dE(cr)(or)(o)])([]),X([M(en),mE(ir)(or)(o)])([]),X([M(en),hE(or)(or)(o)])([]),X([M(en),xE(ur)(or)(o)])([]),X([M(en),yE(ku)(or)(o)])([]),X([M(en),gE(Ou)(or)(o)])([]),X([M(en),wE(_u)(or)(o)])([]),X([M(en),bE(fr)(ur)(o)])([]),X([M(en),$E(cr)(ur)(o)])([]),X([M(en),_E(ir)(ur)(o)])([]),X([M(en),OE(or)(ur)(o)])([]),X([M(en),kE(ur)(ur)(o)])([]),X([M(en),SE(ku)(ur)(o)])([]),X([M(en),TE(Ou)(ur)(o)])([]),X([M(en),PE(_u)(ur)(o)])([])])])})})}}},BE=Mn({reflectType:function(){return`<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})),DE=Wn(ee),RE=nn(),NE=kn(an),Su=_o(mu(On($e()(j(j(be)($o)()()()({reflectSymbol:function(){return"q"}}))(vu)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),zE=function(){return x.value}(),LE=function(n){return function(e){return function(r){return BE(x.value)(zE)({bandpass:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([Kt(DE(a)(RE))(function(u){return NE(.8)([Su({frequency:400,q:1})([u]),Su({frequency:880,q:5})([u]),Su({frequency:1200,q:10})([u]),Su({frequency:2e3,q:20})([u]),Su({frequency:3e3,q:30})([u])])})])}})})}}},ME=xn({reflectType:function(){return`<section>
  <h2 id="compression">Compression</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">Compression</a>, when used judiciously, can make certain sounds sit better in a mix, like for example vocals. The <a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">MDN Web Audio documentation</a> does an excellent job explaining how its parameters work. When used not-judiciously, it makes everything sound loud, and who likes that? So let's use it judiciously, like in the example below. We'll pass an object that only specifies the threshold and otherwise use the default options for the compressor.</p>

  <pre><code>
-- defaultDynamicsCompressor =
--   { ratio: 12.0
--   , attack: 0.003
--   , release: 0.25
--   , knee: 30.0
--   , threshold: -24.0
--   }
run2_
  [ dynamicsCompressor_ { threshold: -50.0 }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~compression~
  </section>
`}})()()(z()(I)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})),WE=kT(RS(On($e()(be))(we()()))),UE=Wn(ee),HE=nn(),jE=function(){return x.value}(),VE=function(n){return function(e){return function(r){return ME(jE)({compression:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([WE({})([UE(a)(HE)])])}})})}}},aa=Pf(),ua=Yi(),GE=ua({reflectSymbol:function(){return"playbackRate"}}),QE=ua({reflectSymbol:function(){return"onOff"}}),YE=ua({reflectSymbol:function(){return"offset"}}),KE=ua({reflectSymbol:function(){return"loopStart"}}),XE=ua({reflectSymbol:function(){return"loopEnd"}}),JE=ua({reflectSymbol:function(){return"gain"}}),ZE=ua({reflectSymbol:function(){return"frequency"}}),nI=ua({reflectSymbol:function(){return"delayTime"}}),Sa=function(){return function(n){var e=GE(x.value),r=su(n);return function(t){return aa(e(r(t)))}}},uc=function(){return function(n){var e=QE(x.value),r=ak(n);return function(t){return aa(e(r(t)))}}},eI=function(){return function(n){var e=YE(x.value),r=su(n);return function(t){return aa(e(r(t)))}}},rI=function(){var n=KE(x.value);return function(e){return aa(n(e))}},tI=function(){var n=XE(x.value);return function(e){return aa(n(e))}},$t=function(){return function(n){var e=JE(x.value),r=su(n);return function(t){return aa(e(r(t)))}}},oc=function(){return function(n){var e=ZE(x.value),r=su(n);return function(t){return aa(e(r(t)))}}},fh=function(){return function(n){var e=nI(x.value),r=su(n);return function(t){return aa(e(r(t)))}}},mc=z(),aI=xn({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(mc(mc(mc(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})),uI=S(lt),oI=kn(an),iI=Hm(fm),cI=ve(Ve(He)),fI=nn(),lI=S(jn(Z)),sI=eI()(Dr),vI=Oi(Ef),pI=$f(_f),dI=function(){return x.value}(),mI=function(n){return function(e){return function(r){return aI(dI)({tf:Te("<|>"),txt:Te(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( pure $ offset $ AudioEnvelope
                  { d: 5.0
                  , o: 0.1
                  , p: 0 .. 1920 # mapWithIndex
                      \\i -> const $
                      if i \`mod\` 3 == 0 then 1.0
                      else 0.0
                  }
              )
          )
      ]
  ]`),constant:dn(r)(n)(function(t){return uI(w)})(function(t){return function(a){return Q(t)([oI(.5)([iI(0)(cI(fI)(lI(sI({d:5,o:.1,p:vI(function(u){return Fn(function(){var o=pI(u)(3)===0;return o?1:0}())})(ct(0)(1920))}))))])])}})})}}},hI=xn({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(z()(I)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})),xI=ne(O1),yI=k(go),gI=uT(Nk),wI=Wn(ee),bI=nn(),$I=function(){return x.value}(),_I=function(n){return function(e){return function(r){return hI($I)({convolution:dn(r)(n)(function(t){return xI(yI(function(a){return function(u){return{loop:a,verb:u}}})(rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(rn(t)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(t){return function(a){return Q(t)([gI(a.verb)([wI(a.loop)(bI)])])}})})}}},OI=Mn({reflectType:function(){return`<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \\b _ -> gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})),kI=Yt(pu),SI=nn(),TI=kn(an),Fo=ac(du),PI=function(){return x.value}(),AI=function(n){return function(e){return function(r){return OI(x.value)(PI)({delay:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(t){return function(a){return Q(t)([Kt(kI(a)(SI))(function(u){return TI(.2)([Fo(.03)([u]),Fo(.1)([u]),Fo(.3)([u]),Fo(.7)([u])])})])}})})}}},EI=xn({reflectType:function(){return`<section>
  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>And here's a little example of a single gain node:</p>

  <pre><code>run2_
  [ gain_ 0.1
    [ loopBuf buf bangOn ]
  ] </code></pre>

  ~gain~
  </section>
`}})()()(z()(I)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})),II=kn(an),qI=Wn(ee),FI=nn(),CI=function(){return x.value}(),BI=function(n){return function(e){return function(r){return EI(CI)({gain:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(t){return function(a){return Q(t)([II(.1)([qI(a)(FI)])])}})})}}},DI=xn({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(z()(I)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})),RI=_l(gl),NI=Wn(ee),zI=nn(),LI=function(){return x.value}(),MI=function(n){return function(e){return function(r){return DI(LI)({highpass:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([RI(2e3)([NI(a)(zI)])])}})})}}},WI=xn({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(z()(I)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})),UI=AT(CS(On($e()(j(j(be)(Xk)()()()({reflectSymbol:function(){return"gain"}}))(Jk)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),HI=Wn(ee),jI=nn(),VI=function(){return x.value}(),GI=function(n){return function(e){return function(r){return WI(VI)({highshelf:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([UI({frequency:2e3,gain:.4})([HI(a)(jI)])])}})})}}},QI=xn({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Ocarina enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(z()(I)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})),YI=cT()()(Rk()(F0)),Ea=fb()(),KI=Wn(ee),XI=nn(),JI=function(){return x.value}(),ZI=function(n){return function(e){return function(r){return QI(JI)({iirFilterEx:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([YI(new q(Ea(20298e-8)(Ea(.0004059599)(Ea(20298e-8)(es))),Ea(1.0126964558)(Ea(-1.9991880801)(Ea(.9873035442)(es)))))([KI(a)(XI)])])}})})}}},nq=Mn({reflectType:function(){return`<section>
  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [ loopBuf
      { buffer: buf
      , playbackRate: 0.5
      , loopStart: 0.1
      , loopEnd: 0.6
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.0
      , loopStart: 0.5
      , loopEnd: 1.2
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.7
      }
      bangOn
  ]</code></pre>

  @loopBuf@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})),lh=$e(),sh=j(j(be)(pm)()()()({reflectSymbol:function(){return"playbackRate"}})),vh={reflectSymbol:function(){return"buffer"}},ph=we()(),Ms=Wn(nc(On(lh(j(j(sh(vm)()()()({reflectSymbol:function(){return"loopStart"}}))(sm)()()()({reflectSymbol:function(){return"loopEnd"}}))(Zi)()()()(vh)))(ph))),hc=nn(),eq=Wn(nc(On(lh(sh(Zi)()()()(vh)))(ph))),rq=function(){return x.value}(),tq=function(n){return function(e){return function(r){return nq(x.value)(rq)({loopBuf:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(t){return function(a){return Q(t)([Ms({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(hc),Ms({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(hc),eq({buffer:a,playbackRate:1.7})(hc)])}})})}}},aq=xn({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(z()(I)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})),uq=Ol(Sm),oq=Wn(ee),iq=nn(),cq=function(){return x.value}(),fq=function(n){return function(e){return function(r){return aq(cq)({lowpass:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([uq(215)([oq(a)(iq)])])}})})}}},lq=xn({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(z()(I)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})),sq=qT(ES(On($e()(j(j(be)(Qk)()()()({reflectSymbol:function(){return"gain"}}))(Yk)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),vq=Wn(ee),pq=nn(),dq=function(){return x.value}(),mq=function(n){return function(e){return function(r){return lq(dq)({lowshelf:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([sq({frequency:91,gain:.4})([vq(a)(pq)])])}})})}}},hq=Mn({reflectType:function(){return`<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use 🎧 when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\\mic -> run2_
  [ case mic of
      Just m -> fix \\i -> gain_ 1.0
        [ microphone m
        , delay_ 0.1 [ gain_ 0.2 [ input i ] ]
        ]
      Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
  ]</code></pre>

  @microphone@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})),xc=kn(an),xq=Dm(cm),yq=ac(du),gq=gT(Ji),wq=function(){return x.value}(),bq=function(n){return function(e){return function(r){return hq(x.value)(wq)({microphone:dn(r)(n)(function(t){return Km(!0)(!1)})(function(t){return function(a){return Q(t)([function(){if(a.microphone instanceof $)return zr(function(u){return xc(1)([xq(a.microphone.value0),yq(.1)([xc(.2)([u])])])});if(a.microphone instanceof _)return xc(.02)([gq(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[a.microphone.constructor.name])}()])}})})}}},$q=xn({reflectType:function(){return`<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ pure $ notch_ { frequency: 880.0, q: 5.0 }
    $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
    $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
    $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
`}})()()(z()(I)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})),Tu=CT(PS(On($e()(j(j(be)(Vk)()()()({reflectSymbol:function(){return"q"}}))(Gk)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),Pu=S(cf),_q=Wn(ee),Oq=nn(),kq=function(){return x.value}(),Sq=function(n){return function(e){return function(r){return $q(kq)({notch:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([Tu({frequency:400,q:1})(Pu(Tu({frequency:880,q:5})(Pu(Tu({frequency:1200,q:10})(Pu(Tu({frequency:2e3,q:20})(Pu(Tu({frequency:3e3,q:30})(Pu(_q(a)(Oq)))))))))))])}})})}}},Tq=xn({reflectType:function(){return`<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
`}})()()(z()(I)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})),Au=NT(SS(On($e()(j(j(j(be)(Uk)()()()({reflectSymbol:function(){return"q"}}))(Hk)()()()({reflectSymbol:function(){return"gain"}}))(jk)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),Eu=S(cf),Pq=Wn(ee),Aq=nn(),Eq=function(){return x.value}(),Iq=function(n){return function(e){return function(r){return Tq(Eq)({peaking:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([Au({frequency:400,q:1,gain:-20})(Eu(Au({frequency:880,q:5,gain:20})(Eu(Au({frequency:1200,q:10,gain:-20})(Eu(Au({frequency:2e3,q:20,gain:20})(Eu(Au({frequency:3e3,q:30,gain:-20})(Eu(Pq(a)(Aq)))))))))))])}})})}}},qq=xn({reflectType:function(){return`<section>
  <h2 id="periodic">Periodic wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">periodic wave oscillator</a> plays back a custom periodic waveform at a given frequency. The custom waveform must be set as part of the initialization and can be changed after initialization. Note that the change will not go into effect if the oscillator is on: it must be turned off and on again.</p>

  <pre><code>\\buf -> run2_
  [
    gain_ 0.2
      [
        periodicOsc
          { frequency: 140.0
          , spec:
              ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                  /\\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
              )
          }
          bangOn
      ]
  ]</code></pre>

  ~periodic~
  </section>
`}})()()(z()(I)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),Fq=S(lt),Cq=kn(an),Bq=bl(yl(On($e()(j(j(be)(hl(vl()))()()()({reflectSymbol:function(){return"spec"}}))(ml)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),Ws=_e(),Us=_e(),Hs=_e(),js=_e(),Dq=nn(),Rq=function(){return x.value}(),Nq=function(n){return function(e){return function(r){return qq(Rq)({periodic:dn(r)(n)(function(t){return Fq(w)})(function(t){return function(a){return Q(t)([Cq(.2)([Bq({frequency:140,spec:new q(Ws(.1)(Us(.2)(Hs(.3)(js(.4)(Bt)))),Ws(.4)(Us(.3)(Hs(.2)(js(.1)(Bt)))))})(Dq)])])}})})}}},zq=xn({reflectType:function(){return`<section>
  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [
    playBuf
      { buffer
      , duration: 3.0
      , bufferOffset: 4.2
      }
      bangOn
  ]
</code></pre>

  ~playBuf~
  </section>
`}})()()(z()(I)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})),Lq=Yt(xl(On($e()(j(j(j(be)(Mk)()()()({reflectSymbol:function(){return"duration"}}))(Lk)()()()({reflectSymbol:function(){return"bufferOffset"}}))(dl)()()()({reflectSymbol:function(){return"buffer"}})))(we()()))),Mq=nn(),Wq=function(){return x.value}(),Uq=function(n){return function(e){return function(r){return zq(Wq)({playBuf:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(t){return function(a){return Q(t)([Lq({buffer:a,duration:3,bufferOffset:4.2})(Mq)])}})})}}},pa=function(){function n(){this.head=null,this.last=null,this.size=0}function e(s,p){this.queue=s,this.value=p,this.next=null,this.prev=null}function r(s){this.draining=!1,this.error=null,this.value=s,this.takes=new n,this.reads=new n,this.puts=new n}var t={};function a(s){try{s()}catch(p){setTimeout(function(){throw p},0)}}function u(s,p){var m=new e(s,p);switch(s.size){case 0:s.head=m;break;case 1:m.prev=s.head,s.head.next=m,s.last=m;break;default:m.prev=s.last,s.last.next=m,s.last=m}return s.size++,m}function o(s){var p;switch(s.size){case 0:return null;case 1:p=s.head,s.head=null;break;case 2:p=s.last,s.head.next=null,s.last=null;break;default:p=s.last,s.last=p.prev,s.last.next=null}return p.prev=null,p.queue=null,s.size--,p.value}function i(s){var p;switch(s.size){case 0:return null;case 1:p=s.head,s.head=null;break;case 2:p=s.head,s.last.prev=null,s.head=s.last,s.last=null;break;default:p=s.head,s.head=p.next,s.head.prev=null}return p.next=null,p.queue=null,s.size--,p.value}function c(s){if(s.queue!==null){if(s.queue.last===s){o(s.queue);return}if(s.queue.head===s){i(s.queue);return}s.prev&&(s.prev.next=s.next),s.next&&(s.next.prev=s.prev),s.queue.size--,s.queue=null,s.value=null,s.next=null,s.prev=null}}function l(s,p){if(!p.draining){var m=p.puts,v=p.takes,f=p.reads,d,h,b,O,g;for(p.draining=!0;;){if(d=null,h=null,b=null,O=p.value,g=f.size,p.error!==null){for(O=s.left(p.error);d=i(m);)a(d.cb(O));for(;h=i(f);)a(h(O));for(;b=i(v);)a(b(O));break}if(O===t&&(d=i(m))&&(p.value=O=d.value),O!==t){for(b=i(v);g--&&(h=i(f));)a(h(s.right(O)));b!==null&&(p.value=t,a(b(s.right(O))))}if(d!==null&&a(d.cb(s.right(void 0))),p.value===t&&m.size===0||p.value!==t&&v.size===0)break}p.draining=!1}}return r.EMPTY=t,r.putLast=u,r.takeLast=o,r.takeHead=i,r.deleteCell=c,r.drainVar=l,r}();function Hq(){return new pa(pa.EMPTY)}function jq(n,e,r){return function(){return r.value===pa.EMPTY&&r.error===null?(r.value=e,pa.drainVar(n,r),!0):!1}}function Vq(n,e){return function(){var r=e.value;return r===pa.EMPTY?n.nothing:(e.value=pa.EMPTY,pa.drainVar(n,e),n.just(r))}}var Gq=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Qq=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),Yq=function(){function n(){}return n.value=new n,n}(),dh=function(){return{left:Qn.create,right:Ln.create,nothing:_.value,just:$.create,killed:Gq.create,filled:Qq.create,empty:Yq.value}}(),Kq=function(n){return function(e){return jq(dh,n,e)}},Xq=function(n){return Vq(dh,n)},Jq=xn({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app 🎙️.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(z()(I)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})),mh=je(),Vs=e0(ru(U)(eu)(mh)),yc=An(Tn),hh=Ve(He),xh=jn(Z),Zq=Al(hh)(xh),yh=Hn(U),Ia=k(yh),Iu=S(gn),gc=Jn(ue),nF=vr(gn)(tr),Gs=r1(n1),eF=Zn(Nt),rF=Zn(Zr),tF=k(go),aF=br(Ge),uF=bt(ge),qu=Ie(Je),oF=Pe(Ee),iF=Be(mh),cF=it(gn)(tr),fF=Pl(ge),lF=Ya(gn),sF=nt(ge),Qs=ne(Xr(Z)),Ys=ve(hh),Ks=S(xh),vF=Ae(yh),pF=function(n){var e=fT(n);return function(r){var t=Dm(r);return function(a){return function(u){return e(u)(t(a))}}}},dF=pF(Dk)(cm),mF=function(){return x.value}(),hF=function(n){return function(e){return function(r){return Jq(mF)({recorder:vn(qn)(function(t){var a=Vs(yc)(t.value1),u=Vs(yc)(function(l){return l.left}(a)),o=function(l){return l.right}(u),i=Zq(r)(function(l){return l.right}(a)),c=function(l){return l.left}(u);return zn([Jr([M("cursor: pointer;"),me(le)(Ia(function(l){if(l.e instanceof qt)return Iu(w);if(l.e instanceof Ft)return gc(gc(gc(l.e.value0)(n(Iu(w))))(nF(l.rec)(function(s){return Gs(Fs(s))})))(t.value0(Ln.create(Ar.value)));if(l.e instanceof Ar)return function(){l.cncl();var p=Hq();t.value0(new Ln(qt.value))();var m=ou(rF(tF(function(v){return v.microphone})(Km(!0)(!1)))(function(v){return aF(function(){var d=pn(Iu(Iu(w)))(function(h){return function(){var O=uF(),g=qu(_a)(),G=kl(O)(),R=qu(J(0))(),Un=qu(J(Ja))(),Nn=function(yn){return yn(G)},B=wl([dF(h)(function(yn){return function(){return t.value0(new Qn(new Ln(yn)))(),oF(Kq(yn)(p))(),LP("audio/ogg; codecs=opus")(function(mn){return t.value0(Qn.create(Qn.create(mn)))})(yn)()}})])(Tl(R)(Un)(Nn)),A=qu(sr(iF(B)(g.event))(Nn))();return g.push(yc)(),function(){qu(A)(),eF(Xq(p))(cF(function(mn){return Gs(Fs(mn))}))();var Sn=fF(O)();return lF(Sn!=="closed")(sF(O))()}}})(v)();return t.value0(new Ln(new Ft(d)))(),d})}))();return n(function(){return t.value0(Ln.create(Ar.value))(),iu(Oo(m))()})(),w};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 60, column 44 - line 105, column 36): "+[l.e.constructor.name])})(Qs(Ia(cp)(Qs(Ia(function(l){return function(s){return function(p){return{e:l,cncl:s,rec:p}}}})(i))(Ys(Ks(Iu(w)))(Ia(function(l){return l.value0})(r)))))(Ys(Ks(_.value))(Ia($.create)(o)))))])([wr(Ia(function(l){if(l instanceof Ar)return"Turn on";if(l instanceof qt)return"Loading...";if(l instanceof Ft)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 115, column 23 - line 118, column 48): "+[l.constructor.name])})(i))]),zn([D$([K1("true"),M("display:none;"),H1(c),eo(vF(c)("display:block;"))])([])])])})})}}},xF=xn({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(z()(I)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),yF=S(lt),gF=kn(an),wF=xT(Bk),bF=nn(),$F=function(){return x.value}(),_F=function(n){return function(e){return function(r){return xF($F)({periodic:dn(r)(n)(function(t){return yF(w)})(function(t){return function(a){return Q(t)([gF(.2)([wF(448)(bF)])])}})})}}},OF=xn({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(z()(I)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),kF=S(lt),SF=kn(an),TF=rc(Ji),PF=nn(),AF=function(){return x.value}(),EF=function(n){return function(e){return function(r){return OF(AF)({periodic:dn(r)(n)(function(t){return kF(w)})(function(t){return function(a){return Q(t)([SF(.2)([TF(448)(PF)])])}})})}}},IF=xn({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(z()(I)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),qF=S(lt),FF=kn(an),CF=$l(pl),BF=nn(),DF=function(){return x.value}(),RF=function(n){return function(e){return function(r){return IF(DF)({periodic:dn(r)(n)(function(t){return qF(w)})(function(t){return function(a){return Q(t)([FF(.2)([CF(448)(BF)])])}})})}}},NF=xn({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(z()(I)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})),zF=DT(Ck),LF=Wn(ee),MF=nn(),WF=function(){return x.value}(),UF=function(n){return function(e){return function(r){return NF(WF)({pan:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(t){return function(a){return Q(t)([zF(1)([LF(a)(MF)])])}})})}}},HF=function(){return x.value}(),jF=xn({reflectType:function(){return`<ul>
  <li><a href="#allpass">All-pass filter</a></li>
  <li><a href="#analyser">Analyser</a></li>
  <li><a href="#bandpass">Bandpass filter</a></li>
  <li><a href="#constant">Constant value</a></li>
  <li><a href="#compression">Compression</a></li>
  <li><a href="#convolution">Convolution</a></li>
  <li><a href="#delay">Delay</a></li>
  <li><a href="#gain">Gain</a></li>
  <li><a href="#highpass">Highpass filter</a></li>
  <li><a href="#highshelf">Highshelf filter</a></li>
  <li><a href="#iir">IIR filter</a></li>
  <li><a href="#loopbuf">Looping buffer</a></li>
  <li><a href="#lowpass">Lowpass filter</a></li>
  <li><a href="#lowshelf">Lowshelf filter</a></li>
  <!--<li><a href="#media">Media element</a></li>-->
  <li><a href="#microphone">Microphone</a></li>
  <li><a href="#notch">Notch filter</a></li>
  <!--<li><a href="#panner">Panner</a></li>-->
  <li><a href="#peaking">Peaking filter</a></li>
  <li><a href="#periodic">Periodic wave oscillator</a></li>
  <li><a href="#playbuf">Playing a buffer</a></li>
  <li><a href="#recorder">Recorder</a></li>
  <li><a href="#sawtooth">Sawtooth wave oscillator</a></li>
  <li><a href="#sine">Sine wave oscillator</a></li>
  <li><a href="#square">Square wave oscillator</a></li>
  <li><a href="#pan">Stereo panner</a></li>
  <li><a href="#triangle">Triangle wave oscillator</a></li>
  <li><a href="#waveshaper">Waveshaper</a></li>
    </ul>
`}})()()(I)(HF)({}),VF=xn({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(z()(I)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),GF=S(lt),QF=kn(an),YF=Lm(im),KF=nn(),XF=function(){return x.value}(),JF=function(n){return function(e){return function(r){return VF(XF)({periodic:dn(r)(n)(function(t){return GF(w)})(function(t){return function(a){return Q(t)([QF(.2)([YF(448)(KF)])])}})})}}},Xs=z(),ZF=xn({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(Xs(Xs(I)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),n5=k(Ce),e5=Hx(yf)(xf),r5=sT($S),t5=Wn(ee),a5=nn(),u5=function(){return x.value}(),o5=function(n){return function(e){return function(r){return ZF(u5)({code:Te(`do
  let
    makeDistortionCurve :: Number -> Array Number
    makeDistortionCurve k =
      map
        ( \\i ->
            let
              x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
            in
              (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
        )
        (0 .. (n_samples - 1))
      where
      n_samples = 44100

      deg = pi / 180.0
  run2_
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`),waveShaper:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(t){return function(a){var u=function(o){var i=ro/180;return n5(function(c){var l=ae(c)*2/ae(44100)-1;return(3+o)*l*20*i/(ro+o*e5(l))})(ct(0)(44099))};return Q(t)([r5(aP(u(400)))([t5(a)(a5)])])}})})}}},i5=Jn(ue),In=z(),c5=xn({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Ocarina. There are only two omissions:</p>
    <ul>
      <li>Audio Worklet Nodes</li>
      <li>Multi-channel audio</li>
    </ul>
    <p>Both of these will be covered in later sections.</p>

  <p>
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here's a table of contents.
  </p>
  ~toc~
  <p>And now, without further ado... (~drumroll~) Here are some audio nodes!</p>

  ~allpass~
  ~analyser~
  ~bandpass~
  ~constant~
  ~compression~
  ~convolution~
  ~delay~
  ~gain~
  ~highpass~
  ~highshelf~
  ~iirFilter~
  ~loopBuf~
  ~lowpass~
  ~lowshelf~
  ~microphone~
  ~notch~
  ~peaking~
  ~periodicOsc~
  ~playBuf~
  ~recorder~
  ~sawtoothOsc~
  ~sinOsc~
  ~squareOsc~
  ~pan~
  ~triangleOsc~
  ~waveShaper~

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>`}})()()(In(In(In(In(In(In(In(In(In(In(In(In(In(In(In(In(Br()(In(In(In(In(In(In(In(In(In(In(In(In(I)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),f5=kn(an),l5=Wn(ee),s5=nn(),v5=function(){return x.value}(),p5=function(n){return function(e){return function(r){return function(t){var a=i5(e(yi.value))(Rr),u=Jt(n)(r);return c5(v5)({drumroll:th("🥁")(t)(u)(function(o){return rn(o)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(o){return function(i){return Q(o)([f5(1)([l5(i)(s5)])])}}),toc:jF,allpass:sA(u)(e)(t),analyser:CE(u)(e)(t),bandpass:LE(u)(e)(t),constant:mI(u)(e)(t),compression:VE(u)(e)(t),convolution:_I(u)(e)(t),delay:AI(u)(e)(t),gain:BI(u)(e)(t),highpass:MI(u)(e)(t),highshelf:GI(u)(e)(t),iirFilter:ZI(u)(e)(t),loopBuf:tq(u)(e)(t),lowshelf:mq(u)(e)(t),lowpass:fq(u)(e)(t),notch:Sq(u)(e)(t),playBuf:Uq(u)(e)(t),peaking:Iq(u)(e)(t),microphone:bq(u)(e)(t),pan:UF(u)(e)(t),periodicOsc:Nq(u)(e)(t),recorder:hF(u)(e)(t),sawtoothOsc:_F(u)(e)(t),sinOsc:EF(u)(e)(t),squareOsc:RF(u)(e)(t),triangleOsc:JF(u)(e)(t),waveShaper:o5(u)(e)(t),next:Xt(t)(a)})}}}},d5=Zn(Nt),Sr=Ie(Je),Js=Ya(gn),m5=gp(yf),Zs=Pe(Ee),h5=Jn(ue),nv=it(gn)(tr),Il=function(n){return function(e){return nu(function(r){return sr(e)(function(t){return function(){var u=Xc(n)();return r({acTime:u,value:t})()}})})}},x5=function(n){var e=function(r){return function(t){return function(a){return function(u){return function(o){return function(i){return function(){var l=Sr(V(a))();return Js(l)(function(){var p=Xc(n)(),m=o0(R2(m5(t-p-.04)(.01)*1e3))(function(){var f=Sr(V(a))();return Js(f)(function(){return Zs(Sr(Yn(new $(t))(o)))(),r(t)(),e(r)(t+i)(a)(u)(o)(i)()})()})();return Zs(Sr(Yn(new $(m))(u)))()})()}}}}}}};return function(){var t=Sr(J(!0))(),a=Sr(J(_.value))(),u=Sr(J(_.value))();return{unsubscribe:h5(Sr(Yn(!1)(t)))(d5(Sr(V(a)))(nv(Yl))),fevent:function(o){return nu(function(i){return sr(o)(function(c){return function(){var s=Sr(V(a))();nv(Yl)(s)();var p=Sr(V(u))(),m=function(){if(p instanceof _)return Xc(n)();if(p instanceof $)return p.value0;throw new Error("Failed pattern match at Ocarina.Clock (line 42, column 17 - line 44, column 29): "+[p.constructor.name])}();return e(i)(m+c)(t)(a)(u)(c)()}})})}}}},xr=function(n){return function(e){return function(r){return function(t){return function(a){var u=r===n||t===e;if(u)return e;var o=(t-e)/(r-n),i=e-o*n;return o*a+i}}}}},ql=function(n){var e=ve(n);return function(r){return function(t){return e(r)(t(w))}}},ev=z(),y5=xn({reflectType:function(){return`<section>
  <h2>Fold</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use four <code>fold</code>-s to store the ambitus and velocity of both vibrato and tremolo.</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. In the example above, instead of having a global state, our two folds allow for two <i>ad hoc</i> local states.</p>

</section>`}})()()(ev(ev(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),gh=je(),Fl=tu(ye)(xe)(gh),g5=Za(Fl),So=Hn(U),Co=Yr(So),w5=ne(Xr(Z)),qa=Ae(So),b5=An(Tn),wh=Ve(He),$5=ve(wh),_5=S(jn(Z)),rv=S(gn),Fu=k(So),O5=bt(ge),k5=hu(ge),S5=Df(Fl),T5=yr(Rt),bh=k0(ye)(gh),P5=Kr(bh)(U)(So),A5=Ie(Je),E5=mo(ye),I5=k(U),tv=$a(Fl),av=Oa(bh)(U)(So),q5=wt(an),F5=$t()(jt),C5=bl(yl(On($e()(j(j(be)(hl(vl()))()()()({reflectSymbol:function(){return"spec"}}))(ml)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),uv=_e(),ov=_e(),iv=_e(),cv=_e(),fv=ql(wh),B5=nn(),D5=oc()(jt),Cu=Jn(ue),R5=nt(ge),N5=k(Ce),z5=function(){return x.value}(),L5=function(n){return function(e){return function(r){return function(t){return y5(z5)({txt:Te(`module Main where

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Poll (sampleBy, sample_, step)
import FRP.Event (memoize)

import FRP.Event.Class (fold, mapAccum, sampleOnRight)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(withACTime)
import Ocarina.Control (gain, periodicOsc)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Math (calcSlope)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type Cbx = V (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , cbx :: Cbx
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                stopE <#> (_ *> push.startStop.start unit)
                startE $> do
                  ctx <- context
                  c0h <- constant0Hack ctx
                  let
                    cevt fast b tm = mapAccum
                      ( \\(oo /\\ act) (pact /\\ pt) ->
                          let
                            tn = pt +
                              ( (act - pact) *
                                  (if oo then fast else 1.0)
                              )
                          in
                            ((act /\\ tn) /\\ tn)
                      )
                      (sampleBy (/\\) b tm)
                      (0.0 /\\ 0.0)

                  r <- run2e ctx
                    ( memoize
                        ( map (add 0.04 <<< _.acTime)
                            $ withACTime ctx animationFrameEvent
                        )
                        \\acTime ->
                          let
                            ev0 = cevt 8.0 cbx0 acTime
                            ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                            ev2 = cevt 4.0 cbx2 acTime
                            ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                            evs f a = sampleOnRight acTime
                              $ map ($)
                              $ sampleOnRight a
                              $ { f: _, a: _, t: _ } <$> f
                          in
                            [ gain 0.0
                                ( evs ev0 ev1 <#> \\{ f, a, t } -> P.gain $ AudioNumeric
                                    { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                                    , o: t
                                    , t: _linear
                                    }
                                )
                                [ periodicOsc
                                    { frequency: 325.6
                                    , spec: (0.3 +> -0.1 +> 0.7 +> -0.4 +> V.empty)
                                        /\\ (0.6 +> 0.3 +> 0.2 +> 0.0 +> V.empty)
                                    }
                                    ( OneOf.do
                                        bangOn
                                        evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                          $ AudioNumeric
                                              { n: 325.6 +
                                                  (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                              , o: t
                                              , t: _linear
                                              }
                                    )
                                ]
                            ]
                    )
                  push.startStop.stop (r *> c0h *> close ctx)
            )
            [ text OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        , D.div
            ( O.oneOfMap (map (attr D.Style)) O.do
                stopE $> "display:block;"
                startE $> "display:none;"
            )
            ( map
                ( \\e -> D.input
                    ( OneOf.do
                        pure (D.Xtype := "checkbox")
                        pure (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`),empl:vn(qn)(function(a){return vn(qn)(function(u){return vn(qn)(function(o){return vn(qn)(function(i){return vn(cu(w))(function(c){return vn(qn)(function(l){var s=function(d){return g5(function(h){return function(b){return!h}})(!1)(d)},p=s(i.value1),m=s(o.value1),v=s(u.value1),f=s(a.value1);return zn([Jr([me(le)(Co(w5(qa(c.value1)(b5))($5(_5(rv(w)))(Fu(function(d){return d.value0})(t))))(function(d){return function(){d();var b=O5(),O=k5(b)(),g=function(B){return function(A){return function(yn){return S5(function(Sn){return function(mn){var bn=Sn.value1+(mn.value1-Sn.value0)*function(){return mn.value0?B:1}();return new q(new q(mn.value1,bn),bn)}})(new q(0,0))(P5(q.create)(A)(yn))}}},G=Fi(),R=A5(ho(E5(I5(function(){var B=T5(.04);return function(A){return B(function(yn){return yn.acTime}(A))}}())(Il(b)(G.event)))))(),Un=Q(b)(function(){var B=function(bn){return function(Me){return tv(R.poll)(Fu(cp)(tv(Me)(Fu(function(We){return function(Y){return function(Gn){return{f:We,a:Y,t:Gn}}}})(bn))))}},A=Fu(function(bn){return bn?4:1})(av(p)(R.poll)),yn=g(4)(m)(R.poll),Sn=Fu(function(bn){return bn?4:1})(av(v)(R.poll)),mn=g(8)(f)(R.poll);return[q5(0)(Co(B(mn)(Sn))(function(bn){return F5({n:xr(1)(.01)(4)(.15)(bn.a)*ds(ro*bn.f)+.15,o:bn.t,t:uo})}))([C5({frequency:325.6,spec:new q(uv(.3)(ov(-.1)(iv(.7)(cv(-.4)(Bt)))),uv(.6)(ov(.3)(iv(.2)(cv(0)(Bt)))))})(fv(B5)(function(){return Co(B(yn)(A))(function(bn){return D5({n:325.6+xr(1)(3)(4)(15.5)(bn.a)*ds(ro*bn.f),o:bn.t,t:uo})})}))])]}())(),Nn=Cu(Cu(Un)(O))(R5(b));return n(Cu(Nn)(c.value0(w)))(),l.value0(Nn)()}})),me(le)(Co(l.value1)(function(d){return Cu(d)(Cu(n(rv(w)))(c.value0(w)))}))])([wr(fv(qa(c.value1)("Turn on"))(function(){return qa(l.value1)("Turn off")}))]),X([eo(qa(l.value1)("display:block;")),eo(qa(c.value1)("display:none;"))])(N5(function(d){return xo([xd("checkbox"),wo(function(h){return d(w)}),X1(qa(c.value1)("false"))])([])})([a.value0,u.value0,o.value0,i.value0]))])})})})})})})})}}}},M5={recip:function(n){return 1/n},Ring0:function(){return xf}},W5=function(n){return function(e){return{EuclideanRing0:function(){return n},DivisionRing1:function(){return e}}}};function ai(n){return function(){return function(e){return n(e)()}}}function ui(n){return function(e){return function(r){return function(t){return function(){return t.addEventListener(n,e,r)}}}}}function oi(n){return function(e){return function(r){return function(t){return function(){return t.removeEventListener(n,e,r)}}}}}function U5(n){return n.clientX}function H5(n){return n.clientY}function af(n){return n.button}var ii=Nf("MouseEvent"),$h=k(Ee),ci=it(gn)(tr),Tt=Pf(),j5=z_(Ka),V5=M_(Ka),G5=function(n){return function(e){return nu(function(r){return sr(e)(function(t){return function(){var u=va(n.buttons)();return r({value:t,buttons:u})()}})})}},Q5=function(){var e=Qe(_.value)(),r=Qe(L_)(),t=$h(d0)(qi)(),a=ai(function(c){return ci(function(l){return ja(new $({x:U5(l),y:H5(l)}))(e)})(ii(c))})(),u=ai(function(c){return ci(function(l){return Uc(j5(af(l)))(r)})(ii(c))})(),o=ai(function(c){return ci(function(l){return Uc(V5(af(l)))(r)})(ii(c))})();ui(Tt("mousemove"))(a)(!1)(t)(),ui(Tt("mousedown"))(u)(!1)(t)(),ui(Tt("mouseup"))(o)(!1)(t)();var i=function(){return oi(Tt("mousemove"))(a)(!1)(t)(),oi(Tt("mousedown"))(u)(!1)(t)(),oi(Tt("mouseup"))(o)(!1)(t)()};return{position:e,buttons:r,dispose:i}},Y5=v0(function(n){return function(){var r=$h(d0)(qi)(),t=ai(function(a){return ci(function(u){return n(af(u))})(ii(a))})();return ui(Tt("mousedown"))(t)(!1)(r)(),oi(Tt("mousedown"))(t)(!1)(r)}}),K5=k(U),X5=function(n){return Xn(function(e){return K5(function(r){return r.value(r.buttons)})(G5(n)(e))})},J5=function(n){return n};function Z5(){return Date.now()}var n4=function(n){return nu(function(e){return sr(n)(function(r){return function(){var a=Z5();return e({time:a,value:r})()}})})},e4=k(U),r4=Xn(function(n){return e4(function(e){return e.value(e.time)})(n4(n))}),lv=k(Hn(U))(function(){var n=h1(x1);return function(e){return n(J5(e))}}())(r4),uf=An(Tn),t4=$a(ye),a4=Ei(ye),u4=zw(ye),Cl=je(),_h=Oa(Cl)(U)(U),o4=W5(Sp)(M5),Bl=Hn(U),lr=k(Bl),sv=ba(),of=ne(Xr(Z)),i4=Mw(Cl)(ye),Oh=Ae(U),vv=z(),c4=xn({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat polls that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-polls</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(vv(vv(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),Fa=ve(Ve(He)),pv=S(jn(Z)),dv=Yr(Bl),wc=Ae(Bl),mv=S(gn),f4=bt(ge),l4=hu(ge),bc=k(Ud),s4=vO(pO),Bo=Zn(jd),v4=S(Gd),p4=_e(),d4=_e(),m4=_e(),h4=_e(),Do=ne(Vd),x4=Ie(Je),y4=mo(ye),g4=k(U),Bu=wt(an),Du=$t()(jt),et=Yd(ll),Ru=gp(yf),Dl=$e(),Rl=j(be),kh={reflectSymbol:function(){return"q"}},Nl={reflectSymbol:function(){return"frequency"}},zl=we()(),w4=Ol(km(On(Dl(j(Rl(Kk)()()()(kh))(lm)()()()(Nl)))(zl))),b4=bT(pl),$c=_o(mu(On(Dl(j(Rl($o)()()()(kh))(vu)()()()(Nl)))(zl))),Ro=bl(yl(On(Dl(j(Rl(hl(vl()))()()()({reflectSymbol:function(){return"spec"}}))(ml)()()()(Nl)))(zl))),No=nn(),zo=oc()(jt),Nu=Jn(ue),$4=nt(ge),_4=ot(he)(gr(xe)),O4=function(n){return function(e){var r=function(i){var c=i.Filterable2().Functor1(),l=Ae(c),s=ew(i),p=Za(i),m=$a(i);return function(v){var f=Be(v),d=Kr(v)(c)(c);return function(h){var b=h.DivisionRing1().Ring0(),O=b.Semiring0(),g=yr(O),G=Dx(O),R=Tp(h.EuclideanRing0()),Un=xp(O),Nn=yp(b);return function(B){var A=yr(B);return function(yn){return function(Sn){return function(mn){return function(bn){var Me=g(G)(G),We=function(Y){return function(Gn){if(Y.last instanceof _)return Gn;if(Y.last instanceof $)return A(Gn)(yn(function(Re){return R(Un(Re(A(Y.last.value0.value1)(Y.now.value1)))(Nn(Y.now.value0)(Y.last.value0.value0)))(Me)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 107, column 5 - line 107, column 35): "+[Y.constructor.name,Gn.constructor.name])}};return Xn(function(Y){var Gn=f(bn)(l(Y)(uf)),Re=s(d(q.create)(mn)(Gn)),_t=p(Kn(We))(Sn)(Re);return m(_t)(Y)})}}}}}}}},t=function(i){var c=r(i);return function(l){var s=c(l);return function(p){return s(p)(p.DivisionRing1().Ring0().Semiring0())(function(m){return m(uf)})}}},a=t(ye)(Cl)(o4),u=function(i){return function(c){return Xn(function(l){return t4(a4(function(s){var p=c(u4(i)(s));return _h(p)(l)}))(l)})}},o=function(i){return function(c){return function(l){return N_(i)?-8*(c-1)-l*2:2*(4-c)}}};return u(2)(function(i){return a(2)(lr(sv)(lv))(function(){var c=u(10)(function(l){return a(10)(lr(sv)(lv))(of(of(lr(o)(X5(n)))(i))(l))});return i4(c)(Oh(e)(c))}())})}},k4=function(){return x.value}(),S4=function(n){return function(e){return function(r){return function(t){return c4(k4)({txt:Te(`module Main

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Poll (APoll, Poll, poll, sample, sampleBy, sample_, step, switcher)
import FRP.Poll.Mouse (buttons)
import FRP.Poll.Time as Time
import FRP.Event (memoize)

import FRP.Event.Class (class IsEvent, fix, fold, sampleOnRight, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- \`swell\` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = ⍺ - βs
--   | mouse up   = ɣ - δs - ε ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using \`solve2'\`.
swell :: Mouse -> Poll Number
swell mouse =
  fixB 2.0 \\b ->
    integral' 2.0 (unwrap <$> Time.seconds)
      let
        db = fixB 10.0 \\db_ ->
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Poll a -> Poll a) -> Poll a
  fixB a fn = poll \\s ->
    fix \\event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOnRight event s }

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The \`Semiring\` \`a\` should be a vector field over the field \`t\`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on \`t\` to a function on \`a\`. Simple examples where \`t ~ a\` can use
  -- | the \`integral'\` function instead.
  integral
    :: forall event a t
     . IsEvent event
    => Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> APoll event t
    -> APoll event a
    -> APoll event a
  integral g initial t b =
    poll \\e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\\) t x)
        z = fold approx y initial
      in
        sampleOnRight z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\\ a1, last: Just (t0 /\\ a0) } s = s + g (\\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of \`integral\` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall event t
     . IsEvent event
    => Field t
    => t
    -> APoll event t
    -> APoll event t
    -> APoll event t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \\push event -> do
      let
        startE = pure unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                event.stop <#> (_ *> push.start unit)
                startE $>
                  do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    mouse <- getMouse
                    ri <- randomInt 0 10000
                    let
                      ttap (o /\\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                      fund = 90.4
                      spcn = map (_ - 0.5) arbitrary
                      spc' = do
                        a <- spcn
                        b <- spcn
                        c <- spcn
                        d <- spcn
                        pure (a +> b +> c +> d +> V.empty)
                      spc = (/\\) <$> spc' <*> spc'
                      spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                      allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                    r <- run2e ctx
                      ( memoize
                          ( map (\\{ acTime, value } -> acTime /\\ value)
                              $ withACTime ctx
                              $ sample_ (swell mouse) animationFrameEvent
                          )
                          \\swm ->
                            [ gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm
                                )
                                [ lowpass_ { frequency: fund, q: 20.0 }
                                    [ squareOsc_ fund ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: (fund * 3.02)
                                        , spec: allSpcs.s0
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 5.07
                                        , spec: allSpcs.s1
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 7.13
                                        , spec: allSpcs.s2
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm
                                )
                                [ periodicOsc
                                    { frequency: fund * 9.14
                                    , spec: allSpcs.s3
                                    }
                                    ( bangOn <|>
                                        ( P.frequency
                                            <<< ttap
                                            <<< second (\\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm
                                        )
                                    )
                                ]
                            ]
                      )
                    push.stop (r *> c0h *> close ctx)
            )
            [ text $ OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        ]
  )`),empl:vn(qn)(function(a){return vn(qn)(function(u){var o=Fa(pv(w))(a.value1);return zn([Jr([me(le)(dv(of(wc(o)(uf))(Fa(pv(mv(w)))(lr(function(i){return i.value0})(t))))(function(i){return function(){i();var l=f4(),s=l4(l)(),p=Q5(),m=Ld(0)(1e4)(),v=function(B){return{o:B.value0+.04,n:B.value1,t:uo}},f=bc(function(B){return B-.5})(s4),d=Bo(f)(function(B){return Bo(f)(function(A){return Bo(f)(function(yn){return Bo(f)(function(Sn){return v4(p4(B)(d4(A)(m4(yn)(h4(Sn)(Bt)))))})})})}),h=Do(bc(q.create)(d))(d),b=Do(Do(Do(bc(function(B){return function(A){return function(yn){return function(Sn){return{s0:B,s1:A,s2:yn,s3:Sn}}}}})(h))(h))(h))(h),O=Hd(b)({newSeed:Md(m),size:5}),g=Fi(),G=Y5(),R=x4(ho(y4(g4(function(B){return new q(B.acTime,B.value)})(Il(l)(_h(O4(p)(Oh(G.event)(w)))(g.event))))))(),Un=Q(l)([Bu(0)(lr(function(){var B=et(function(A){return Ru(-.4)(.5*(A-1))});return function(A){return Du(v(B(A)))}}())(R.poll))([w4({frequency:90.4,q:20})([b4(90.4)])]),Bu(0)(lr(function(){var B=et(function(A){return Ru(-.2)(.4*(A-3))});return function(A){return Du(v(B(A)))}}())(R.poll))([$c({frequency:90.4*4,q:20})([Ro({frequency:90.4*3.02,spec:O.s0})(Fa(No)(lr(function(){var B=et(function(A){return 273.00800000000004+14*(A-1)});return function(A){return zo(v(B(A)))}}())(R.poll)))])]),Bu(0)(lr(function(){var B=et(function(A){return Ru(-.1)(.2*(A-6))});return function(A){return Du(v(B(A)))}}())(R.poll))([$c({frequency:90.4*6,q:20})([Ro({frequency:90.4*5.07,spec:O.s1})(Fa(No)(lr(function(){var B=et(function(A){return 458.32800000000003+18*(A-1)});return function(A){return zo(v(B(A)))}}())(R.poll)))])]),Bu(0)(lr(function(){var B=et(function(A){return Ru(0)(.2*(A-3))});return function(A){return Du(v(B(A)))}}())(R.poll))([$c({frequency:90.4*8,q:20})([Ro({frequency:90.4*7.13,spec:O.s2})(Fa(No)(lr(function(){var B=et(function(A){return 644.552+32*(A-1)});return function(A){return zo(v(B(A)))}}())(R.poll)))])]),Bu(0)(lr(function(){var B=et(function(A){return Ru(0)(.1*(A-7))});return function(A){return Du(v(B(A)))}}())(R.poll))([Ro({frequency:90.4*9.14,spec:O.s3})(Fa(No)(lr(function(){var B=et(function(A){return 826.2560000000001+31*(A-1)});return function(A){return zo(v(B(A)))}}())(R.poll)))])])(),Nn=Nu(Nu(Un)(s))($4(l));return n(Nu(Nn)(a.value0(w)))(),u.value0(Nn)()}})),me(le)(dv(u.value1)(function(i){return Nu(i)(Nu(n(mv(w)))(a.value0(w)))}))])([wr(_4([wc(o)("Turn on"),wc(u.value1)("Turn off")]))])])})})})}}}},hv=z(),T4=xn({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(hv(hv(Br()(I)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})),P4=Jn(ue),A4=function(){return x.value}(),E4=function(n){return function(e){return function(r){return function(t){var a=Jt(n)(r);return T4(A4)({next:Xt(t)(P4(e(Kc.value))(Rr)),fold:L5(a)(e)(r)(t),fix:S4(a)(e)(r)(t)})}}}},Sh=ve(Ve(He)),Ll=jn(Z),Th=S(Ll),I4=$t()(Dr),q4=wt(an),F4=rc(Ji),xv=z(),C4=Mn({reflectType:function(){return`<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur <span style="font-weight:800;">now</span>, that is to say, immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node. To construct one, use a record with the following parameters:<ul><li><code>p</code>: a list of numbers that will be interpolated over.</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li><li><code>d</code>: the duration of the envelope.</li></ul></li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off. To construct one, use a record with the following parameters:<ul><li><code>n</code>: an enum with the value <code>_on</code>, <code>_off</code> or <code>_onOff</code> (more on this in <a href="#example3">Example 3</a> below).</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li></ul></li>
  </ul>

  <p>After that, in the example below, it's functions all the way down. <code>oon</code> and <code>oof</code> create our on/off events, <code>env</code> creates our gain envelope, <code>ooo'</code> and <code>env'</code> specialize these envelopes to a specific point in time, and <code>cell</code> creates a single cell that we deploy 100 times.</p>

  <p>One important thing to note here is the use of the tie fighter (<code>&lt;|&gt;</code>), aka <code>alt</code>, in the definition of <code>ooo'</code>. The <code>Event</code> type, when <code>alt</code>'d, preserves a before-after relationship of the left and right operands when the operands happen at the same time. This is a bit hackish: the events conceptually happen at the same time, but on our CPU, one has to follow the other. We can use this, however, to make sure that certain events happen in a logical sequence. For example, an <code>off</code> instruction must be issued after an <code>on</code> instruction, which we guarantee by using <code>oon</code> on the left side of the alt. If we did it the other way, the <code>on</code> instruction would be last and we'd wind up with 100 oscillators playing at the same time!</p>

  <p>A last thing to note before the music plays is how scheduling works here. Even though all the events are issued upfront via <code>bang</code>, they schedule things to be played <i>later</i> in the audio context. We'll see more advanced scheduling techniques in the <a href="#example4"><code>requestAnimationFrame</code> example below</a>.</p>

  <pre><code>@txt@</code></pre>

  @ex0@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
`}})({reflectType:function(){return"@"}})()()(xv(xv(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})),Ph=Hn(U),yv=Yr(Ph),B4=ne(Xr(Z)),gv=k(Ph),wv=S(gn),_c=Jn(ue),D4=kn(an),R4=Dt(co),N4=k(Ce),z4=function(){function n(){}return n.value=new n,n}(),bv=function(){function n(){}return n.value=new n,n}(),Oc=function(){function n(e){this.value0=e}return n.create=function(e){return new n(e)},n}(),L4=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pow)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\\))
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = pure $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = pure $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \\i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 \`pow\` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 \`pow\` i)))
    env' x = env (x + 0.3 * (i * (1.005 \`pow\` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody Deku.do
  push /\\ event <- useState Init
  D.div_
    [ D.button
        [ DL.runOn DL.click $ event <#> case _ of
            Stop u -> u *> push Start
            _ -> do
              r <- run2_
                [ gain_ 1.0
                    -- we create 100 cells
                    $ join
                    $ cell <$> 0 .. 100
                ]
              push $ Stop r
        ]
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ]
`,M4=function(){return x.value}(),W4=function(n){var e=S(n);return function(r){var t=uc()(Ki);return function(a){return e(t({x:um,o:a}))}}},U4=W4(Ll)(),H4=function(n){var e=S(n);return function(r){var t=uc()(Ki);return function(a){return e(t({x:vk,o:a}))}}},j4=H4(Ll)(),V4=Di(au)(ae)(function(n){var e=function(a){return Sh(U4(a+.27*(n*ri(1.005)(n))))(j4(a+3+.3*(n*ri(1.005)(n))))},r=function(a){return Th(I4({p:[0,.4,.1,.05,.01,0],o:a+.3*(n*ri(1.005)(n)),d:.8}))},t=function(a){return function(u){return q4(0)(r(a))([F4(200+n*u)(e(a))])}};return[t(.2)(4),t(.3)(6),t(.45)(14),t(.7)(20)]}),G4=function(n){return function(e){return function(r){return C4(x.value)(M4)({txt:Te(L4),ex0:vn(cu(z4.value))(function(t){return zn([Jr([me(le)(yv(B4(gv(q.create)(t.value1))(Sh(Th(wv(w)))(gv(function(a){return a.value0})(r))))(function(a){return a.value0 instanceof Oc?_c(_c(a.value0.value0)(t.value0(bv.value)))(n(wv(w))):function(){a.value1();var o=ah([D4(1)(R4(N4(V4)(ct(0)(100))))])();return n(_c(o)(t.value0(bv.value)))(),t.value0(new Oc(o))()}}))])([wr(yv(t.value1)(function(a){return a instanceof Oc?"Turn off":"Turn on"}))])])})})}}},kc=z(),Q4=Mn({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Ocarina bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>Ocarina.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Ocarina parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(kc(kc(kc(I)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),Y4=Wn(nc(On($e()(j(j(j(j(be)(pm)()()()({reflectSymbol:function(){return"playbackRate"}}))(vm)()()()({reflectSymbol:function(){return"loopStart"}}))(sm)()()()({reflectSymbol:function(){return"loopEnd"}}))(Zi)()()()({reflectSymbol:function(){return"buffer"}})))(we()()))),Ah=Ve(He),Lo=ql(Ah),K4=nn(),Ml=Hn(U),zu=k(Ml),X4=Sa()(am),J4=rI(),Z4=tI(),$v=ne(Xr(Z)),nC=yr(Rt),_v=ve(Ah),Ov=S(jn(Z)),eC=se(ma),rC=k(Ce),tC=it(gn)(tr),aC=lf(Nt),uC=bi(gf),Mo=Ae(Ml),Sc=S(gn),kv=Yr(Ml),Wo=Jn(ue),oC=An(Tn),Tc=Zn(Zr),iC=bt(Ge),cC=hu(Ge),fC=br(Ge),lC=nt(ge),sC=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)

import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall payload
     . Maybe BrowserAudioBuffer
    -> Nut Effect payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> pure unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              OneOf.do
                bangOn
                map
                  (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                  sl0
                map
                  (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                  sl1
                map
                  (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                  (biSampleOn sl2 (add <$> (pure 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap pure O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb
                          ( traverse_
                              (valueAsNumber >=> f)
                              <<< (=<<) fromEventTarget
                              <<< target
                          )
                    )
                    []
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    start $> (music >>= push.startStop.stop)
                    event.startStop.stop <#>
                      (_ *> push.startStop.start unit)
                )
                [ text OneOf.do
                    start $> "Turn on"
                    event.startStop.stop $> "Turn off"
                ]
            ]
`,vC=function(){return x.value}(),pC="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",dC=function(n){return function(e){return function(r){return Q4(x.value)(vC)({wagtxt:Te(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ OneOf.do
      bangOn
      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (pure 0.0 <|> sl1))`),txt:Te(sC),ex1:vn(qn)(function(t){return vn(qn)(function(a){return vn(qn)(function(u){return vn(cu(w))(function(o){return vn(qn)(function(i){return vn(qn)(function(c){var l=function(s){return Y4({buffer:s,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Lo(K4)(function(){return Lo(zu(function(){var p=xr(0)(.2)(100)(5);return function(m){return X4(p(m))}}())(t.value1))(function(){return Lo(zu(function(){var p=xr(0)(0)(100)(1.2);return function(m){return J4(p(m))}}())(a.value1))(function(){return zu(function(){var p=xr(0)(.05)(100)(1);return function(m){return Z4(p(m))}}())(function(p){return $v(p)(u.value1)}(zu(nC)(_v(Ov(0))(a.value1))))})})}))};return zn(eC(rC(function(s){return zn([Te(s.l),xo([Li,Ui("0"),Hi("100"),Wi("1"),Mi("50"),ji(function(){var p=tC(aC(zf)(s.f)),m=uC(Lf);return function(v){return p(m(al(v)))}}())])([])])})([{l:"Playback rate",f:t.value0},{l:"Loop start",f:a.value0},{l:"Loop end",f:u.value0}]))([Jr([me(le)(Mo(c.value1)(Sc(w))),me(le)(kv(i.value1)(function(s){return Wo(s)(Wo(n(Sc(w)))(o.value0(w)))})),me(le)(kv($v(Mo(o.value1)(oC))(_v(Ov(Sc(w)))(zu(function(s){return s.value0})(r))))(function(s){return function(){s(),c.value0(w)();var m=ou(Tc(iC)(function(v){return Tc(cC(v))(function(f){return Tc(rn(v)(pC))(function(d){return fC(function(){var b=Q(v)([l(d)])(),O=Wo(Wo(b)(f))(lC(v));return i.value0(O)(),O})})})}))();return n(function(){return o.value0(w)(),iu(Oo(m))()})(),w}}))])([wr(Lo(Mo(i.value1)("Turn off"))(function(){return Mo(o.value1)("Turn on")}))])]))})})})})})})})}}},Sv=z(),mC=Mn({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Ocarina comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Ocarina <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in ocarina is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The ocarina interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The ocarina interval needs an audio context to work.</li>
    <li>The ocarina interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom poll called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-ctiming, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the cas a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(Sv(Sv(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})),Wl=Hn(U),dr=k(Wl),hC=yr(Rt),Tv=ve(Ve(He)),xC=nn(),yC=oc()(am),gC=Lm(im),wC=kn(an),Pc=wt(an),Ac=$t()(Dr),Eh=$e(),Ih=j(be),qh={reflectSymbol:function(){return"q"}},Fh={reflectSymbol:function(){return"frequency"}},Ch=we()(),Pv=_o(mu(On(Eh(j(Ih($o)()()()(qh))(vu)()()()(Fh)))(Ch))),bC=_l(Tm(On(Eh(j(Ih(Zk)()()()(qh))(dm)()()()(Fh)))(Ch))),$C=it(gn)(tr),_C=lf(Nt),OC=bi(gf),Av=Yr(Wl),Ev=ne(Xr(Z)),Ec=Ae(Wl),kC=An(Tn),SC=S(jn(Z)),Iv=S(gn),TC=bt(ge),qv=Ie(Je),PC=ka(Z),fa=Jn(ue),Fv=nt(ge),AC=ot(he)(gr(xe)),EC=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Poll (Poll, poll, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Poll Number
random = poll \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        start = event.startStop.start <|> pure unit

        music :: forall lock. _ -> Event (Array (Audible _ _))
        music evt' = memoize evt' \\evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \\ipt _ -> do
              gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
      D.div_
        [ D.div_
            [ text_ "tempo"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb
                      ( traverse_
                          (valueAsNumber >=> push.slider)
                          <<< (=<<) fromEventTarget
                          <<< target
                      )
                )
                []
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ start $> do
                    ctx <- context
                    let
                      myIvl = sampleBy Tuple random
                        $ interval ctx 0.91
                        $ map (calcSlope 0.0 0.42 100.0 1.4)
                        $ event.slider
                    r <- run2e ctx (music myIvl)
                    push.startStop.stop (r *> close ctx)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ start $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
  )`,IC=Xn(function(n){return nu(function(e){return sr(n)(function(r){return function(){var a=ao();return e(r(a))()}})})}),qC=function(){return x.value}(),FC=function(n){return n<.142857?261.625565:n<.285714?293.664768:n<.428571?349.228231:n<.571429?391.995436:n<.714286?440:n<.857143?523.251131:587.329536},CC=function(n){return function(e){return function(r){return mC(x.value)(qC)({txt:Te(EC),ex2:vn(cu(w))(function(t){return vn(qn)(function(a){return vn(qn)(function(u){var o=function(i){var c=dr(function(){var f=hC(.01);return function(d){return f(Xa(d))}}())(i),l=dr(Yu)(i),s=Tv(xC)(dr(function(f){return yC(FC(f))})(l)),p=dr(function(f){return sc(function(d){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:d}}(f))})(c),m=dr(function(f){return sc(function(d){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:d}}(f))})(c),v=dr(function(f){return sc(function(d){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:d}}(f))})(c);return[Kt(gC(0)(s))(function(f){return wC(2)([Pc(0)(dr(Ac)(v))([Pv({frequency:1e3,q:20})([f])]),Pc(0)(dr(Ac)(m))([Pv({frequency:2e3,q:20})([f])]),Pc(0)(dr(Ac)(p))([bC({frequency:4e3,q:20})([f])])])})]};return zn([zn([Te("tempo"),xo([Li,Ui("0"),Hi("100"),Wi("1"),Mi("50"),ji(function(){var i=$C(_C(zf)(u.value0)),c=OC(Lf);return function(l){return i(c(al(l)))}}())])([])]),Jr([me(le)(Av(Ev(Ec(t.value1)(kC))(Tv(SC(Iv(w)))(dr(function(i){return i.value0})(r))))(function(i){return function(){i();var l=TC(),s=x5(l)(),p=qv(ho(Ev(dr(q.create)(IC))(PC(s.fevent)(dr(xr(0)(.42)(100)(1.4))(u.value1)))))(),m=Q(l)(o(p.poll))(),v=fa(m)(Fv(l));return n(fa(v)(t.value0(w)))(),a.value0(fa(fa(fa(v)(s.unsubscribe))(qv(p.unsubscribe)))(Fv(l)))()}})),me(le)(Av(a.value1)(function(i){return fa(i)(fa(n(Iv(w)))(t.value0(w)))}))])([wr(AC([Ec(t.value1)("Turn on"),Ec(a.value1)("Turn off")]))])])})})})})}}},BC=function(){return x.value}(),DC=function(){return Mn({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(I)(x.value)(BC)({})}(),RC=function(){return x.value}(),NC=function(){return Mn({reflectType:function(){return`<section>
  <h2>Events in Ocarina</h2>
  <p>Ocarina follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

  <pre><code>sinOsc
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload</code></pre>

  <p>In that defintion, <code>SinOsc</code> is a <code>newtype</code> that is defined as such:</p>

  <pre><code>newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))</code></pre>

  <p>Informally, the <code>sinOsc</code> function listens to an event that contains one of several possible instructions, including changing the frequency and turning the oscillator on or off. Let's see a few more examples:</p>

    <table>
    <tr>
      <th>Audio Unit</th>
      <th>Newtype name</th>
      <th>Variant</th>
    </tr>
    <tr>
      <td><code>delay</code></td>
      <td><code>Delay</code></td>
      <td><code>Variant (delayTime :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>highshelf</code></td>
      <td><code>Highshelf</code></td>
      <td><code>Variant (frequency :: AudioParameter, gain :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>playBuf</code></td>
      <td><code>PlayBuf</code></td>
      <td><code>Variant (playbackRate :: AudioParameter, buffer :: BrowserAudioBuffer, duration :: Maybe Number, bufferOffset :: Number, onOff :: AudioOnOff)</code></td>
    </tr>
  </table>

  <p>In practice, you'll never need to use newtypes presented above. The <code>Ocarina.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


    <table>
    <tr>
      <th>Constructor</th>
      <th>Type</th>
      <th>Receptive Audio Units</th>
    </tr>
    <tr>
      <td><code>bufferOffset</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>delayTime</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>delay</code></li></ul></td>
    </tr>
    <tr>
      <td><code>duration</code></td>
      <td><code>Maybe Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>frequency</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowpass</code></li>
          <li><code>lowshelf</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>gain</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>gain</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowshelf</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>loopEnd</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>loopStart</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>offset</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>onOff</code></td>
      <td><code>AudioOnOff</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
          <li><code>loopBuf</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>playBuf</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>playbackRate</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>loopBuf</code></li><li>playBuf</li></ul></td>
    </tr>
    <tr>
      <td><code>q</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>lowpass</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
  </table>

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Ocarina can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Ocarina:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Ocarina. Idiomatic Ocarina projects use functional reactive programming as a way to "steer" web audio, and Ocarina aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(I)(x.value)(RC)({})}(),zC=function(){return x.value}(),LC=function(){return Mn({reflectType:function(){return`<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Poll</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype APoll event a =
  APoll (forall b. event (a -> b) -> event b)
type Poll = APoll Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will bthe Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Poll</h3>

  <p>
    The <code>Poll</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to una <code>b</code>) and unlocks it with an <code>a</code>. Polls don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Poll (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a poll, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the poll of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(I)(x.value)(zC)({})}(),MC=Jn(ue),Ca=z(),WC=Mn({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Ocarina.
  </p>

  @primer@
  @inOcarina@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Poll</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(Ca(Ca(Ca(Ca(Ca(Br()(Ca(I)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})),UC=function(){return x.value}(),HC=function(n){return function(e){return function(r){return function(t){var a=function(o){return Xt(t)(MC(e(o))(Rr))},u=Jt(n)(r);return WC(x.value)(UC)({next:a(Qc.value),primer:LC,inOcarina:NC,flavors:DC,ex0:G4(u)(e)(t),ex1:dC(u)(e)(t),ex2:CC(u)(e)(t)})}}}},jC=Mn({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = pure $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),VC=yo(st),Ic=ne(el),GC=k(zi),Uo=uu(st),QC=kn(an),YC=S(jn(Z)),KC=uc()(Ki),XC=rm(),JC=yr(Rt),Ho=Yt(pu),ZC=function(){return x.value}(),nB=function(n){return function(e){return function(r){return jC(x.value)(ZC)({ai0:dn(r)(n)(function(t){return VC(Ic(Ic(Ic(GC(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})(Uo(rn(t)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Uo(rn(t)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Uo(rn(t)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Uo(rn(t)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(t){return function(a){return Q(t)([QC(1)(function(){var u=function(o){return YC(KC(XC(JC(o))(sl)))};return[Ho(a.tink0)(u(.1)),Ho(a.tink1)(u(.2)),Ho(a.tink2)(u(.9)),Ho(a.tink3)(u(1.8))]}())])}})})}}},eB=Mn({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = pure $ onOff $ dt (add n) apOn
            mtk i =
              case i \`mod\` 4 of
                0 -> tink0
                1 -> tink1
                2 -> tink2
                _ -> tink3
          0 .. 100 &lt;#&gt;
            \\i' -> do
              let i = toNumber i'
              playBuf (mtk i')
                (ooo (0.3 + 0.3 * (i * (1.005 \`pow\` i))))</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),rB=yo(st),qc=ne(el),tB=k(zi),jo=uu(st),aB=kn(an),uB=S(jn(Z)),oB=uc()(Ki),iB=rm(),cB=yr(Rt),fB=$f(_f),lB=Yr(Ce),sB=Yt(pu),vB=function(){return x.value}(),pB=function(n){return function(e){return function(r){return eB(x.value)(vB)({ai0:dn(r)(n)(function(t){return rB(qc(qc(qc(tB(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})(jo(rn(t)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(jo(rn(t)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(jo(rn(t)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(jo(rn(t)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(t){return function(a){return Q(t)([aB(1)(function(){var u=function(i){return uB(oB(iB(cB(i))(sl)))},o=function(i){var c=fB(i)(4);return c===0?a.tink0:c===1?a.tink1:c===2?a.tink2:a.tink3};return lB(ct(0)(100))(function(i){var c=ae(i);return sB(o(i))(u(.3+.3*(c*ri(1.005)(c))))})}())])}})})}}},dB=Mn({reflectType:function(){return`<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),mB=Wn(ee),hB=nn(),xB=kn(an),Lu=_o(mu(On($e()(j(j(be)($o)()()()({reflectSymbol:function(){return"q"}}))(vu)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),yB=function(){return x.value}(),gB=function(n){return function(e){return function(r){return dB(x.value)(yB)({ai0:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([Kt(mB(a)(hB))(function(u){return xB(.8)([Lu({frequency:400,q:1})([u]),Lu({frequency:880,q:5})([u]),Lu({frequency:1200,q:10})([u]),Lu({frequency:2e3,q:20})([u]),Lu({frequency:3e3,q:30})([u])])})])}})})}}},wB=Mn({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \\i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),bB=Wn(ee),$B=nn(),_B=kn(an),OB=Yr(Ce),kB=Di(au),SB=_o(mu(On($e()(j(j(be)($o)()()()({reflectSymbol:function(){return"q"}}))(vu)()()()({reflectSymbol:function(){return"frequency"}})))(we()()))),TB=function(){return x.value}(),PB=function(n){return function(e){return function(r){return wB(x.value)(TB)({ai0:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([Kt(bB(a)($B))(function(u){return _B(.8)(OB(ct(0)(40))(kB(ae)(function(o){return SB({frequency:200+o*150,q:30})([u])})))})])}})})}}},AB=Mn({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),Cv=kn(an),EB=Yt(pu),IB=nn(),qB=ac(du),FB=function(){return x.value}(),CB=function(n){return function(e){return function(r){return AB(x.value)(FB)({ai0:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(t){return function(a){return Q(t)([zr(function(u){return Cv(1)([EB(a)(IB),qB(.1)([Cv(.6)([u])])])})])}})})}}},Bv=z(),BB=Mn({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(Bv(Bv(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),DB=Yt(pu),RB=nn(),Dv=kn(an),Rv=wt(an),Bh=jn(Z),NB=function(){return x.value}(),zB=function(n){var e=S(n);return function(r){return e($t()(Dr)({p:[1,1,0],o:0,d:10}))}},LB=zB(Bh)(),MB=function(n){var e=S(n);return function(r){return e($t()(Dr)({p:[1,1,0],o:0,d:8}))}},WB=MB(Bh)(),UB=function(n){var e=ac(n);return function(r){var t=kn(r);return function(a){var u=_l(a);return function(o){return function(i){return function(c){return function(l){return e(o)([t(i)([u(c)(l)])])}}}}}}},Ba=UB(du)(an)(gl),HB=function(n){return function(e){return function(r){return BB(x.value)(NB)({txt:Te(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  [ fan1 (playBuf buf bangOn) \\b _ -> mix $ fix
      \\g0 -> gain_ 1.0
        [ b
        , dgh 0.15 0.7 1500.0
            [ fix
                \\g1 -> gain 1.0 fade1
                  [ dgh 0.4 0.5 2500.0
                      [ g0, g1 ]
                  ]
            ]
        , dgh 0.29 0.85 2000.0
            [ fix
                \\g1 -> gain_ 1.0
                  [ dgh 0.6 0.6 3500.0
                      [ g0
                      , ( fix
                            \\g2 -> gain 1.0 fade0
                              [ dgh 0.75 0.6 4000.0
                                  [ g1, g2 ]
                              , dgh 0.75 0.55 3000.0 [ b ]
                              ]
                        )
                      ]
                  ]
            ]
        ]
  ]`),ai0:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(t){return function(a){return Q(t)([Kt(DB(a)(RB))(function(u){return zr(function(o){return Dv(1)([u,Ba(.15)(.7)(1500)([zr(function(i){return Rv(1)(LB)([Ba(.4)(.5)(2500)([o,i])])})]),Ba(.29)(.85)(2e3)([zr(function(i){return Dv(1)([Ba(.6)(.6)(3500)([o,zr(function(c){return Rv(1)(WB)([Ba(.75)(.6)(4e3)([i,c]),Ba(.75)(.55)(3e3)([u])])})])])})])])})})])}})})}}},jB=Jn(ue),VB=Mn({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how ocarina handles all three cases:</p>
    <ul>
      <li><span style="font-weight:800px;"><code>Array</code></span> is the data structure we'll use to send many audio units into one.</li>
      <li><span style="font-weight:800px;"><code>fan</code></span> is a function that we'll use to "fan" one audio node out to many.</li>
      <li><span style="font-weight:800px;"><code>fix</code></span> is the function we'll use to make an audio unit an input into itself.</li>
    </ul>
    <h2>The setup</h2>
    <p>
      To illustrate how <code>Array</code>, <code>fan</code> and <code>fix</code> work, we're going to use a few new audio units.
    </p>
      <ul>
        <li><code>delay</code>: A delay node</li>
        <li><code>bandpass</code>: A bandpass filter, meaning a filter that lets a single frequency band pass through.</li>
        <li><code>playBuf</code> and <code>loopBuf</code>: Playback or looping playback of a buffer. We'll use some MP3 files from freesound.org.</li>
      </ul>
  </section>`}})({reflectType:function(){return"@"}})()()(Br()(I)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})),GB=function(){return x.value}(),QB=function(n){return function(e){return function(r){return function(t){var a=function(u){return Xt(t)(jB(e(u))(Rr))};return VB(x.value)(GB)({hwLink:a(xi.value)})}}}},YB=Jn(ue),la=z(),KB=Mn({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Ocarina graph</h3>

  @intro@

  <h2>Arrays</h2>

  <p>To send several audio units through one, we use an <code>Array</code>.</p>

  @code0@

  <p>PureScript <code>Array</code>-s are extremely flexible and efficient, so go to town! For example, you can <code>map</code> (aka <code>&lt;#&gt;</code> when flipped) over a range of integers to create audio units, like in the example below.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes a vector of audio signals and fans it out to multiple processing chains. If you have a single signal, you can use <code>fan1</code>. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>A vector of nodes to fan out (or a single node in the case of <code>fan1</code>).</li>
    <li>A function that accepts a reference to this/these node(s) and returns a new node that may or may not contain the input.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself 🤯. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Ocarina doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section 🎸</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(la(la(la(la(la(la(la(Br()(I)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})),XB=function(){return x.value}(),JB=function(n){return function(e){return function(r){return function(t){var a=function(o){return Xt(t)(YB(e(o))(Rr))},u=Jt(n)(r);return KB(x.value)(XB)({intro:QB()(e)(r)(t),next:a(Vc.value),code0:nB(u)(e)(t),code1:pB(u)(e)(t),code2:gB(u)(e)(t),code3:PB(u)(e)(t),code4:CB(u)(e)(t),code5:HB(u)(e)(t)})}}}},ZB=Jn(ue),Nv=z(),nD=Mn({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Ocarina. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  @code@
  @result@

  <h2>The <code>run</code> functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to register an unsubscribe effect. When we trigger the effect it in the <code>Just</code> branch of our pattern match, the audio turns off.</p>

  <h2>Audio units</h2>

  <p>The sound you hear when you play the example above is created with the statement <code>gain_ 0.15 [ sinOsc 440.0 bangOn ]</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.15</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.15</code> is roughly <code>-16.5 dB</code>.</p>

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Ocarina. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Ocarina graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(Nv(Br()(Nv(I)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),eD=S(lt),rD=kn(an),tD=rc(Ji),aD=nn(),uD=function(){return x.value}(),oD=function(n){return function(e){return function(r){return function(t){var a=ZB(e(Gc.value))(Rr),u=Jt(n)(r);return nD(x.value)(uD)({code:q$([C$([Te(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])]),result:dn(t)(u)(function(o){return eD(w)})(function(o){return function(i){return Q(o)([rD(.15)([tD(440)(aD)])])}}),next:Xt(t)(a)})}}}},iD=jf,cD=function(){return function(n){return n}},fD=function(){return function(n){return n}};function zv(n){return function(){return n.getContext("2d")}}function Fc(n){return function(e){return function(){n.fillStyle=e}}}function lD(n){return function(){n.beginPath()}}function sD(n){return function(){n.fill()}}function vD(n){return function(e){return function(){n.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function Lv(n){return function(e){return function(){n.fillRect(e.x,e.y,e.width,e.height)}}}var Dh=xa(ha),pD=Mn({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),Ul=Hn(U),mr=k(Ul),dD=ka(Z),Rh=$e(),Nh=j(be),zh=we()(),mD=Nm(Bm(On(Rh(j(Nh(Cm)()()()({reflectSymbol:function(){return"fftSize"}}))(Fm)()()()({reflectSymbol:function(){return"cb"}})))(zh))),Cc=S(gn),hD=S(cf),xD=Yt(pu),Mv=ve(Ve(He)),yD=nn(),gD=Sa()(jt),Da=Yd(ll),Wv=kn(an),wD=tc(Pm(On(Rh(j(Nh(nS)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(mm)()()()({reflectSymbol:function(){return"delayTime"}})))(zh))),Vo=fh()(jt),Bc=wt(an),bD=$t()(jt),Lh=gr(xe),St=Ir(Lh),To=jn(Z),Dc=it(gn)(tr),$D=lf(Nt),_D=bi(gf),Uv=ot(he)(Lh),Hv=Ae(Ul),jv=Yr(Ul),Ra=Jn(ue),OD=ne(Xr(Z)),kD=An(Tn),SD=S(To),Go=Zn(Zr),TD=bt(Ge),PD=hu(Ge),AD=k(go),ED=fD(),ID=v1(st)(iD),qD=cD(),Vv=br(Ge),FD=bb(),CD=po(If)(gn),Gv=Ie(Je),BD=vr(gn)(tr),DD=k(Ee),RD=k(Ce),ND=uh(),zD=nt(ge),LD=function(){return 2*ro}(),Na=function(n){return{o:n.value0+.04,n:n.value1,t:uo}},MD=function(){return x.value}(),WD=function(n){var e=S(n);return function(r){var t=oc()(Dr);return function(a){return function(u){return e(t({p:[a,u],o:0,d:16}))}}}},za=WD(To)(),UD=function(n){var e=S(n);return function(r){return e($t()(Dr)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},HD=UD(To)(),jD=function(n){var e=S(n);return function(r){return e($t()(Dr)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}},VD=jD(To)(),GD=function(n){var e=tc(n);return function(r){var t=wt(r);return function(a){var u=Um(a);return function(o){return function(i){return function(c){return function(l){return function(s){return function(p){return function(m){return e(o)(i)([t(c)(l)([u(s)(p)(m)])])}}}}}}}}}},Qo=GD(du)(an)(gl),QD=function(n){var e=tc(n);return function(r){var t=wt(r);return function(a){var u=Wm(a);return function(o){return function(i){return function(c){return function(l){return function(s){return function(p){return function(m){return e(o)(i)([t(c)(l)([u(s)(p)(m)])])}}}}}}}}}},Qv=QD(du)(an)(LS),YD=function(n){var e=S(n);return function(r){var t=fh()(Dr);return function(a){return function(u){return e(t({p:[a,u],o:0,d:16}))}}}},KD=YD(To)(),Mh=400,Rc=ae(Mh),XD=function(){return Dh(Mh)+"px"}(),Wh=600,Nc=ae(Wh),JD=function(){return Dh(Wh)+"px"}(),ZD={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},nR=function(n){return function(e){return function(r){return pD(x.value)(MD)({ex1:vn(cu(w))(function(t){return vn(qn)(function(a){return vn(qn)(function(u){return vn(qn)(function(o){return vn(qn)(function(i){var c=function(l){return function(s){return function(p){var m=mr(function(v){return new q(v.acTime,v.value)})(dD(Il(l))(o.value1));return[mD({cb:function(v){return function(){return ja(new $(v))(p)(),ja(_.value)(p)}},fftSize:Xd.value})(hD(Kt(xD(s)(Mv(yD)(mr(function(){var v=Da(xr(0)(.96)(100)(1.04));return function(f){return gD(Na(v(f)))}}())(m))))(function(v){return zr(function(f){return Wv(1)([v,wD({maxDelayTime:2.5,delayTime:1})(mr(function(){var d=Da(xr(0)(.5)(100)(2.45));return function(h){return Vo(Na(d(h)))}}())(m))([Bc(.4)(mr(function(){var d=Da(xr(0)(.6)(100)(.9));return function(h){return bD(Na(d(h)))}}())(m))([v])]),Qo(.15)(St)(.7)(St)(1500)(za(1500)(3e3))([zr(function(d){return Bc(1)(HD)([Qo(.4)(St)(.5)(St)(3e3)(za(3e3)(100))([f,d])])})]),Qo(.29)(mr(function(){var d=Da(xr(0)(.1)(100)(.4));return function(h){return Vo(Na(d(h)))}}())(m))(.85)(St)(2e3)(za(2e3)(5e3))([zr(function(d){return Wv(1)([Qo(.6)(mr(function(){var h=Da(xr(0)(.8)(100)(.3));return function(b){return Vo(Na(h(b)))}}())(m))(.6)(St)(3500)(za(3500)(100))([f,zr(function(h){return Bc(1)(VD)([Qv(.75)(mr(function(){var b=Da(xr(0)(.9)(100)(.1));return function(O){return Vo(Na(b(O)))}}())(m))(.6)(St)(4e3)(za(4e3)(200))([d,h]),Qv(.75)(KD(.75)(.2))(.55)(St)(200)(za(200)(4e3))([v])])})])])})])])})})))]}}};return zn([B$([M1(JD),Q1(XD),M("width: 100%;"),N1(function(){var l=Dc(function(s){return function(){var m=zv(s)();return Fc(m)("black")(),Lv(m)({width:Nc,height:Rc,x:0,y:0})(),w}});return function(s){return l(Jl(s))}}()),od(mr(function(l){var s=Dc(function(p){return function(){var v=zv(p)();return Fc(v)("black")(),Lv(v)({width:Nc,height:Rc,x:0,y:0})(),Fc(v)("rgba(255,255,255,0.2)")(),$i(l)(function(f){return function(){return lD(v)(),vD(v)({end:LD,radius:f.value1*40,start:0,x:f.value0.x*Nc,y:f.value0.y*Rc,useCounterClockwise:!1})(),sD(v)()}})()}});return function(p){return s(Jl(p))}})(i.value1))])([]),xo([Li,Ui("0"),Hi("100"),Wi("1"),Mi("50"),M("width: 100%;"),ji(function(){var l=Dc($D(zf)(o.value0)),s=_D(Lf);return function(p){return l(s(al(p)))}}())])([]),Jr([Uv([M("width:100%; padding:1.0rem;"),me(le)(Hv(u.value1)(Cc(w))),me(le)(jv(a.value1)(function(l){return Ra(l)(Ra(n(Cc(w)))(t.value0(w)))})),me(le)(jv(OD(Hv(t.value1)(kD))(Mv(SD(Cc(w)))(mr(function(l){return l.value0})(r))))(function(l){return function(){l(),u.value0(w)();var p=Qe(_.value)(),m=ou(Go(TD)(function(v){return Go(PD(v))(function(f){return Go(AD(ED)(ID(rn(v))(qD(ZD))))(function(d){return Go(Vv(Ld(0)(5e4)))(function(h){var b=Hd(sO(bp(eb(d.pluck0))(rb(N0(FD(d))))))({newSeed:Md(h),size:4});return Vv(function(){var g=CD(function(B){return function(){var yn=ao(),Sn=ao();return{x:yn,y:Sn}}})(ct(0)(127))(),G=Q(v)(c(v)(b)(p))(),R=Fi(),Un=Gv(sr(R.event)(function(B){return function(){var yn=va(p)();return BD(yn)(function(Sn){return function(){var bn=Ym(Sn)(),Me=DD(function(){var We=tg(g),Y=RD(function(Gn){return function(Re){return Re/255}(Gn)});return function(Gn){return We(Y(Gn))}}())(ND(bn))();return i.value0(Me)(),w}})()}}))(),Nn=Ra(Ra(Ra(Ra(G)(f))(zD(v)))(Gv(Un)))(R.unsubscribe);return a.value0(Nn)(),Nn})})})})}))();return n(function(){return t.value0(w)(),iu(Oo(m))()})(),w}}))])])([wr(Uv([mr(Fn("Turn off"))(a.value1),mr(Fn("Turn on"))(t.value1),mr(Fn("Loading..."))(u.value1)]))])])})})})})})})}}},eR=xn({reflectType:function(){return`<div>
  <h1>Ocarina</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-ocarina">Ocarina</a>.</p>

  <p>Ocarina is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Ocarina tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Ocarina that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-ocarina/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world à la ocarina</a>!</p>
</div>`}})()()(z()(Br()(I)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})),rR=Jn(ue),tR=function(){return x.value}(),aR=function(n){return function(e){return function(r){return function(t){var a=Jt(n)(r);return eR(tR)({next:Xt(t)(rR(e(xi.value))(Rr)),ex:nR(a)(e)(t)})}}}},uR=xn({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Ocarina allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in ocarina looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(Br()(I)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),oR=Jn(ue),iR=function(){return x.value}(),cR=function(n){return function(e){return function(r){return function(t){return uR(iR)({next:wo(function(a){return oR(e(jm.value))(Rr)})})}}}},Yv=z(),fR=xn({reflectType:function(){return`<section>
  <h2>Cancel</h2>
  <p>The <code>AudioCancel</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues"><code>cancelScheduledValues</code></a> function and cancels whatever effects you programmed in the future. In the example below, we execute the following sequence:</p>
  <ol>
    <li>Play an audio file</li>
    <li>Send an event at 1.0 seconds to schedule an evenlope to modulate the audio rate starting at 1.5 seconds.</li>
    <li>Cancel the envelope at 3.0 seconds, but schedule the cancelation to take effect at 4.0 seconds.</li>
  </ol>
  <pre><code>~txt~</code></pre>
  ~cancel~
  </section>
`}})()()(Yv(Yv(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),lR=kn(an),sR=Wn(ee),vR=ot(he)(gr(xe)),pR=nn(),Kv=ka(Z),Xv=S(jn(Z)),Uh=Sa(),dR=Uh(Dr),mR=Dt(co),hR=Ae(Ce),xR=Uh(xk),yR=function(){return x.value}(),gR=function(n){return function(e){return function(r){return fR(yR)({txt:Te(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ pure
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (pure (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`),cancel:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([lR(1)([sR(a)(vR([pR,Kv(Qa(1e3))(Xv(dR({p:mR(hR(ct(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Kv(Qa(3e3))(Xv(xR({o:3.5})))]))])])}})})}}},Jv=z(),wR=xn({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(Jv(Jv(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})),bR=kn(an),$R=Wn(ee),_R=ot(he)(gr(xe)),OR=nn(),kR=ka(Z),SR=S(jn(Z)),TR=Sa()(Dr),PR=Dt(co),AR=Ae(Ce),ER=function(){return x.value}(),IR=function(n){return function(e){return function(r){return wR(ER)({txt:Te(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ pure
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`),envelope:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([bR(1)([$R(a)(_R([OR,kR(Qa(1e3))(SR(TR({p:PR(AR(ct(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})})}}},qR=Mn({reflectType:function(){return`<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric</code> encompasses the following three functions from the Web Audio API:</p>

  <ul>
    <li><code>linearRampToValueAtTime</code> via the <code>_linear</code> transition.</li>
    <li><code>exponentialRampToValueAtTime</code> via the <code>_exponential</code> transition.</li>
    <li><code>setValueAtTime</code> via the <code>_step</code> transition.</li>
  </ul>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})),FR=kn(an),CR=Wn(ee),Yo=ql(Ve(He)),BR=nn(),Zv=ka(Z),Ko=S(jn(Z)),Xo=Sa()(jt),DR=function(){return x.value}(),RR=function(n){return function(e){return function(r){return qR(x.value)(DR)({numericEx:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([FR(1)([CR(a)(Yo(BR)(function(){return Yo(Zv(Qa(1e3))(Yo(Ko(Xo({n:1,o:1,t:Ts})))(function(){return Ko(Xo({n:1.3,o:2,t:uo}))})))(function(){return Zv(Qa(2500))(Yo(Ko(Xo({n:1,o:2.5,t:Ts})))(function(){return Ko(Xo({n:.7,o:3.5,t:dk}))}))})}))])])}})})}}},NR=Mn({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ pure
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(z()(I)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})),zR=kn(an),LR=Wn(ee),MR=ot(he)(gr(xe)),WR=nn(),UR=ka(Z),HR=S(jn(Z)),jR=Sa()(sk),VR=function(){return x.value}(),GR=function(n){return function(e){return function(r){return NR(x.value)(VR)({suddenEx:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([zR(1)([LR(a)(MR([WR,UR(Qa(1500))(HR(jR({n:1.4})))]))])])}})})}}},QR=xn({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      pure
        $ playbackRate
        $ c1
            ( gain_ 1.0
                [ constant 1.0 bangOn
                , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                ]
            )
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(z()(I)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})),YR=Wn(ee),KR=ot(he)(gr(xe)),zc=nn(),XR=S(jn(Z)),JR=Sa()(fk()),np=kn(an),ZR=Hm(fm),n8=Ol(Sm),e8=$l(pl),r8=function(){return x.value}(),t8=function(n){return function(e){return function(r){return QR(r8)({unitEx:dn(r)(n)(function(t){return rn(t)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(t){return function(a){return Q(t)([YR(a)(KR([zc,XR(JR(ok(np(1)([ZR(1)(zc),np(.2)([n8(100)([e8(50)(zc)])])]))))]))])}})})}}},a8=Jn(ue),Mu=z(),u8=xn({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how ocarina exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(Mu(Mu(Br()(Mu(Mu(Mu(I)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),o8=function(){return x.value}(),i8=function(n){return function(e){return function(r){return function(t){var a=a8(e(Yc.value))(Rr),u=Jt(n)(r);return u8(o8)({sudden:GR(u)(e)(t),numeric:RR(u)(e)(t),envelope:IR(u)(e)(t),cancel:gR(u)(e)(t),unit:t8(u)(e)(t),next:Xt(t)(a)})}}}},c8=xn({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of ocarina (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(Br()(I)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),f8=Jn(ue),l8=function(){return x.value}(),s8=function(n){return function(e){return function(r){return function(t){return c8(l8)({next:wo(function(a){return f8(e(Vm.value))(Rr)})})}}}},v8=xn({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Ocarina. The units are:</p>

  <ul>
    <li>A wicked sawtooth synth.</li>
    <li>A kickin' comb filter.</li>
    <li>Freeverb, the free-software-world's favorite reverb!</li>
  </ul>

  <h2>Faust</h2>

  <p>Here are our three faust examples:</p>

  <h3>Sawtooth</h3>

  <h3>Comb filter</h3>

  <h3>Freeverb</h3>

  <h3>Compilation</h3>

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in ocarina-land.

  <pre><code>placeholder</code></pre>

  <h2>Ocarina</h2>

  <p>Ocarina provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, ocarina will enforce it for you. Let's see how in the following example. Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h3>The API contract</h3>

  <p>The <code>InitializeAudioWorklet</code> API contract is defined like so:</p>

  <pre><code>InitializeAudioWorkletNode
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }</code></pre>

  <p>And here is how the APIs parameters are used:</p>

  <table>
    <tr>
      <th>Parameter</th>
      <th>Description</th>
    </tr>
    <tr>
      <td><code>name</code></td>
      <td>The name of the worklet as defined in the JS output. When in doubt, check the JS produced by faust!</td>
    </tr>
    <tr>
      <td><code>numerOfInputs</code></td>
      <td>How many inputs does the node take? Note that this is <i>not</i> the number of channels, but rather the number of distinct inputs, like in a merger node.</td>
    </tr>
    <tr>
      <td><code>numerOfOutputs</code></td>
      <td>How many outputs does the node produce? Note that this is <i>not</i> the number of channels, but rather the number of distinct outputs, like in a splitter node.</td>
    </tr>
    <tr>
      <td><code>outputChannelCount</code></td>
      <td>How many channels are in each output? This is defined using a tuple of type-level numbers.</td>
    </tr>
    <tr>
      <td><code>parameterData</code></td>
      <td>Initial parameters for the audio node. This will also define what parameters can be sent to it via events (the record gets flipped to a <a href="https://github.com/natefaubion/purescript-variant"><code>Variant</code></a> when it is an event).</td>
    </tr>
  </table>

  <h2>Next steps</h2>
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via ocarina. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by ocarina. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there 😅</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a ocarina graph</a>.</p>
</div>`}})()()(Br()(I)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),p8=Jn(ue),d8=function(){return x.value}(),m8=function(n){return function(e){return function(r){return function(t){return v8(d8)({next:wo(function(a){return p8(e(yi.value))(Rr)})})}}}},h8=Mn({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(I),x8=function(){return x.value}(),y8=function(n){return function(e){return function(r){return function(t){return h8(x.value)(x8)({})}}}},ep=z(),g8=Mn({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(ep(ep(I)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),rp=ve(Ve(He)),tp=S(jn(Z)),Hl=je(),ic=Hn(U),w8=Kr(k0(ye)(Hl))(U)(ic),Hh=tu(ye)(xe)(Hl),b8=Za(Hh),$8=kn(an),Jo=k(ic),Lc=Ae(ic),_8=Ct(ru(U)(eu)(Hl)),O8=k(ya),k8=ka(Z),S8=zt(Hh),T8=Yt(xl(On($e()(j(j(be)(Wk)()()()({reflectSymbol:function(){return"playbackRate"}}))(dl)()()()({reflectSymbol:function(){return"buffer"}})))(we()()))),P8=nn(),Mc=S(gn),ap=Yr(ic),Zo=Jn(ue),A8=ne(Xr(Z)),E8=An(Tn),Wc=Zn(Zr),I8=bt(Ge),q8=hu(Ge),F8=br(Ge),C8=nt(ge),B8=ot(he)(gr(xe)),D8=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)

import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Poll (Poll, poll, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)

import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (Channel(..), dyn, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

random :: Poll Number
random = poll \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall payload
     . Maybe BrowserAudioBuffer
    -> Nut Effect payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ dyn $ map
                  ( \\i ->
                      OneOf.do
                        pure $ sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ pure $ silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb (const (push.slider unit))
                )
                []
            ]
        , D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                startE $> (music >>= push.startStop.stop)
                event.startStop.stop <#>
                  (_ *> push.startStop.start unit)
            )
            [ text OneOf.do
                startE $> "Turn on"
                event.startStop.stop $> "Turn off"
            ]
        ]
`,R8=Xn(function(n){return nu(function(e){return sr(n)(function(r){return function(){var a=ao();return e(r(a))()}})})}),N8=function(){return x.value}(),z8="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",L8=function(n){return function(e){return function(r){return g8(x.value)(N8)({txt:Te(D8),ex1:vn(qn)(function(t){return vn(qn)(function(a){return vn(qn)(function(u){return vn(qn)(function(o){var i=rp(tp(w))(t.value1),c=w8(q.create)(R8)(b8(function(s){return function(p){return s+1|0}})(0)(o.value1)),l=function(s){return[$8(1)([E0(Jo(function(p){return new q(Lc(_8(function(){var m=O8(Yu);return function(v){return m(wf(v))}}())(k8(f0(5e3))(S8(c))))(uk),T8({buffer:s,playbackRate:.7+Yu(p)*2})(P8))})(c))])]};return zn([zn([Te("Slide me!"),xo([Li,Ui("0"),Hi("100"),Wi("1"),Mi("50"),ji(function(s){return o.value0(w)})])([])]),Jr([me(le)(Lc(u.value1)(Mc(w))),me(le)(ap(a.value1)(function(s){return Zo(s)(Zo(n(Mc(w)))(t.value0(w)))})),me(le)(ap(A8(Lc(i)(E8))(rp(tp(Mc(w)))(Jo(function(s){return s.value0})(r))))(function(s){return function(){s(),u.value0(w)();var m=ou(Wc(I8)(function(v){return Wc(q8(v))(function(f){return Wc(rn(v)(z8))(function(d){return F8(function(){var b=ah(l(d))(),O=Zo(Zo(b)(f))(C8(v));return a.value0(O)(),O})})})}))();return n(function(){return t.value0(w)(),iu(Oo(m))()})(),w}}))])([wr(B8([Jo(Fn("Turn off"))(a.value1),Jo(Fn("Turn on"))(i)]))])])})})})})})}}},up=z(),M8=xn({reflectType:function(){return`<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do a round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise mechansim to dynamically insert audio graphs based on events.
  </p>

  ~suby~

  <h2>Go forth and be brilliant!</h2>
  <p>Thus ends the first version of the ocarina documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Ocarina!</p>
</div>`}})()()(up(up(I)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})),W8=kn(an),U8=Wn(ee),H8=nn(),j8=function(){return x.value}(),V8=function(n){return function(e){return function(r){return function(t){var a=Jt(n)(r);return M8(j8)({appl:th("👏")(t)(a)(function(u){return rn(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(o){return Q(u)([W8(1)([U8(o)(H8)])])}}),suby:L8(a)(e)(t)})}}}},jh=je(),G8=Za(tu(ye)(xe)(jh)),Q8=S(gn),Y8=k(Ce),K8=k(Hn(U)),op=zg(ru(U)(eu)(jh)),hr=function(n){return vn(qn)(function(e){return n(e.value0)(e.value1)})},X8=function(){var n=function(e){var r=function(t){if(t instanceof qo)return zn([hr(aR(e.setCancellation)(e.setPage))]);if(t instanceof xi)return zn([hr(oD(e.setCancellation)(e.setPage))]);if(t instanceof Gc)return zn([hr(JB(e.setCancellation)(e.setPage))]);if(t instanceof Vc)return zn([hr(p5(e.setCancellation)(e.setPage))]);if(t instanceof jm)return zn([hr(m8(e.setCancellation)(e.setPage))]);if(t instanceof yi)return zn([hr(HC(e.setCancellation)(e.setPage))]);if(t instanceof Qc)return zn([hr(i8(e.setCancellation)(e.setPage))]);if(t instanceof Yc)return zn([hr(E4(e.setCancellation)(e.setPage))]);if(t instanceof Vm)return zn([hr(y8(e.setCancellation)(e.setPage))]);if(t instanceof WT)return zn([hr(cR(e.setCancellation)(e.setPage))]);if(t instanceof Kc)return zn([hr(V8(e.setCancellation)(e.setPage))]);if(t instanceof UT)return zn([hr(s8(e.setCancellation)(e.setPage))]);throw new Error("Failed pattern match at Ocarina.Example.Docs (line 133, column 5 - line 133, column 71): "+[t.constructor.name])};return r(e.page)};return vn(cu(new bu(qo.value)))(function(e){var r=G8(Kn(function(t){if(t instanceof bu)return function(a){return{prevPage:new $(a.curPage),curPage:t.value0,cancel:a.cancel,pageChange:!0}};if(t instanceof qs)return function(a){return{cancel:t.value0,pageChange:!1,curPage:a.curPage,prevPage:a.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 51, column 13 - line 53, column 81): "+[t.constructor.name])}))({prevPage:_.value,curPage:qo.value,cancel:Q8(w),pageChange:!0})(e.value1);return zn([zn(Y8(function(t){return E$([R$([wo(function(a){return e.value0(new bu(t.value0))}),M("cursor:pointer;"),me(le)(K8(function(a){return function(){return a.cancel(),e.value0(new bu(t.value0))()}})(op(function(a){return!function(u){return u.pageChange}(a)})(r)))])([Te(t.value1.value0)]),Jf([M(function(){return t.value1.value1?"":"display:none;"}())])([Te(" | ")])])})([new q(qo.value,new q("Home",!0)),new q(xi.value,new q("Hello world",!0)),new q(Gc.value,new q("Array, fan, and fix",!0)),new q(Vc.value,new q("Audio units",!0)),new q(yi.value,new q("Events",!0)),new q(Qc.value,new q("Parameters",!0)),new q(Yc.value,new q("State",!0)),new q(Kc.value,new q("Subgraphs",!1))])),p2(op(function(t){return t.pageChange})(r))(function(t){return n({page:t.curPage,setPage:function(a){return e.value0(bu.create(a))},setCancellation:function(a){return e.value0(qs.create(a))}})})])})}(),J8=x_(X8);J8();
