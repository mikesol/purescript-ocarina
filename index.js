function jb(t){return()=>t.slice()}function Xb(t){return r=>e=>()=>{e[t]=r}}function Qb(t){return()=>t.slice()}var Kb=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var Yb=Kb,Zb=Kb;var ty=function(t){return function(r){return t===r}};var ry=ty,ey=ty;var y=function(){function t(){}return t.value=new t,t}();var be=function(t){return t.reflectSymbol};var bl=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},wa=function(t){return function(r){return r[t]}},Gu=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var yl={eq:ey},Ii={eq:ry};var ar=function(t){return t.eq};var tr=function(){function t(){}return t.value=new t,t}(),dr=function(){function t(){}return t.value=new t,t}(),br=function(){function t(){}return t.value=new t,t}();var ny=function(t){return function(r){return t-r|0}},ay=function(t){return function(r){return t-r}};var uy=function(t){return function(r){return t+r|0}},oy=function(t){return function(r){return t*r|0}},iy=function(t){return function(r){return t+r}},fy=function(t){return function(r){return t*r}};var ya=function(t){return t.zero};var Aa={add:iy,zero:0,mul:fy,one:1},xu={add:uy,zero:0,mul:oy,one:1};var ka=function(t){return t.one};var Pn=function(t){return t.mul};var Lr=function(t){return t.add};var Fu=function(t){return t.sub};var Df={sub:ay,Semiring0:function(){return Aa}},ym={sub:ny,Semiring0:function(){return xu}};var Al=function(t){return function(r){return Fu(t)(ya(t.Semiring0()))(r)}};var Pa=function(){return{compare:Zb(tr.value)(br.value)(dr.value),Eq0:function(){return yl}}}(),Je=function(){return{compare:Yb(tr.value)(br.value)(dr.value),Eq0:function(){return Ii}}}();var rr=function(t){return t.compare};var ly=function(t){return function(r){return function(e){var n=rr(t)(r)(e);return!(n instanceof tr)}}};var Ou=function(t){return function(r){return function(e){var n=rr(t)(r)(e);if(n instanceof tr)return e;if(n instanceof br||n instanceof dr)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var km=function(t){return function(r){return function(e){var n=ly(t)(e)(ya(r.Semiring0()));return n?e:Al(r)(e)}}};var _y=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,o=0;o<e;o++)for(var i=t[o],m=0;m<n;m++)a[u++]=i(r[m]);return a}};var Zo={compose:function(t){return function(r){return function(e){return t(r(e))}}}},$u=function(t){return t.compose};var Z=function(t){return t.identity},tt={identity:function(t){return t},Semigroupoid0:function(){return Zo}};var ne=!0;var St=function(t){return function(r){return function(e){return t(e)(r)}}},x=function(t){return function(r){return t}};var Ri=function(t){return function(r){return r(t)}},Yf=function(t){return function(r){return t(r)}};var py=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var p=function(t){return t.map},Yr=function(t){return function(r){return function(e){return p(t)(e)(r)}}},yr=function(t){return p(t)(x(void 0))},Q=function(t){return function(r){return function(e){return p(t)(x(e))(r)}}},Zf=function(t){return function(r){return p(t)(x(r))}};var ca={map:$u(Zo)},Fr={map:py},gm=function(t){return function(r){return function(e){return p(t)(function(n){return n(e)})(r)}}};var gl={apply:_y,Functor0:function(){return Fr}},It=function(t){return t.apply};var X=function(t){return function(r){return function(e){return It(t)(p(t.Functor0())(x(Z(tt)))(r))(e)}}},Qn=function(t){return function(r){return function(e){return function(n){return It(t)(p(t.Functor0())(r)(e))(n)}}}};var _=function(t){return t.pure};var In=function(t){return function(r){return function(e){if(r)return e;if(!r)return _(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,e.constructor.name])}}},Cl=function(t){return function(r){return function(e){return It(t.Apply0())(_(t)(r))(e)}}};var _e={pure:function(t){return[t]},Apply0:function(){return gl}};var sy=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var te=function(t){return t.discard};var ti={bind:sy,Apply0:function(){return gl}},W=function(t){return t.bind},Kn=function(t){return St(W(t))};var tc=function(t){return function(r){return function(e){return function(n){return W(t)(r(n))(e)}}}};var Gr={discard:function(t){return W(t)}};var Va=function(t){return function(r){return W(t)(r)(Z(tt))}};var _t=function(t){return t};var my=_t;var vy=function(t){return function(r){return function(){return t(r())}}};function En(t){return function(){return{value:t}}}var je=function(t){return function(){return t.value}},Dy=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},Rn=function(t){return function(r){return function(){return r.value=t}}};var Ju=function(t){return function(r){return function(e){return W(t.Bind1())(r)(function(n){return W(t.Bind1())(e)(function(a){return _(t.Applicative0())(n(a))})})}}};var dy=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var bf=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var by={append:function(t){return function(r){return void 0}}};var hn={append:dy};var dt=function(t){return t.append},hm=function(t){return{append:function(r){return function(e){return function(n){return dt(t)(r(n))(e(n))}}}}};var N=function(t){return t.alt};var JT=String.fromCharCode(65535),jT=String.fromCharCode(0),XT=Number.POSITIVE_INFINITY,QT=Number.NEGATIVE_INFINITY;var Yn=function(t){return t.top};var yf={top:2147483647,bottom:-2147483648,Ord0:function(){return Je}};var Zn=function(t){return t.bottom};var Ay=function(t){return t.toString()},ky=function(t){var r=t.toString();return isNaN(r+".0")?r:r+".0"};var ip={show:ky},Ja={show:Ay};var Gt=function(t){return t.show};var z=function(){function t(){}return t.value=new t,t}(),R=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Zt=function(t){return function(r){return function(e){if(e instanceof z)return t;if(e instanceof R)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Me={map:function(t){return function(r){return r instanceof R?new R(t(r.value0)):z.value}}};var ga=function(t){return Zt(t)(Z(tt))},ra=function(){return function(t){if(t instanceof R)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Li={apply:function(t){return function(r){if(t instanceof R)return p(Me)(t.value0)(r);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Me}},Ca={bind:function(t){return function(r){if(t instanceof R)return r(t.value0);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Li}};var Eo=function(){return{pure:R.create,Apply0:function(){return Li}}}();var Xt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Qt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var kf={map:function(t){return function(r){if(r instanceof Xt)return new Xt(r.value0);if(r instanceof Qt)return new Qt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[r.constructor.name])}}};var Ia=function(t){return function(r){return function(e){if(e instanceof Xt)return t(e.value0);if(e instanceof Qt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},fp=function(){return Ia(x(z.value))(R.create)}();var mu=function(t){return t};var ho={map:function(t){return function(r){return t(r)}}};var gy={apply:function(t){return function(r){return t(r)}},Functor0:function(){return ho}},iS={bind:function(t){return function(r){return r(t)}},Apply0:function(){return gy}},Sm={pure:mu,Apply0:function(){return gy}},Xu={Applicative0:function(){return Sm},Bind1:function(){return iS}};var Cy=function(t){return Math.min(Math.abs(t),2147483647)},Ey=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},hy=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Ty=function(t){return function(r){return t/r}};var Sy={Ring0:function(){return Df}},xy={Ring0:function(){return ym}};var ja=function(t){return t.mod};var Tl={degree:function(t){return 1},div:Ty,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return Sy}},Qu={degree:Cy,div:Ey,mod:hy,CommutativeRing0:function(){return xy}},Ku=function(t){return t.div};var we={mempty:void 0,Semigroup0:function(){return by}};var $t=function(t){return t.mempty},Xe=function(t){return{mempty:function(r){return $t(t)},Semigroup0:function(){return hm(t.Semigroup0())}}};var xm=function(t){return function(){return t}},Fy=function(t){return function(r){return function(){return r(t())()}}};var Sl=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var Oy=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},ri={Applicative0:function(){return l},Bind1:function(){return Tn}},Tn={bind:Fy,Apply0:function(){return Fm(0)}},l={pure:xm,Apply0:function(){return Fm(0)}},$y=Oy("functorEffect","Effect",function(){return{map:Cl(l)}}),Fm=Oy("applyEffect","Effect",function(){return{apply:Ju(ri),Functor0:function(){return $y(0)}}}),F=$y(20),at=Fm(23),My=function(t){return{append:Qn(at)(dt(t))}},Ae=function(t){return{mempty:xm($t(t)),Semigroup0:function(){return My(t.Semigroup0())}}};var wy=function(t){return function(){return{value:t}}};var Pe=function(t){return function(){return t.value}},Py=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},Sn=function(t){return function(r){return function(){r.value=t}}};var pe=wy,sS=Py,nc=function(t){return sS(function(r){var e=t(r);return{state:e,value:e}})},gf=function(t){return function(r){return yr(F)(nc(t)(r))}};var bS=Dy,Mu=function(t){return bS(function(r){var e=t(r);return{state:e,value:e}})},To={map:vy};var d={liftST:my,Monad0:function(){return ri}},Rt=function(t){return t.liftST};var ln=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},AS=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},kS=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},cp=typeof Array.prototype.fill=="function"?AS:kS,gS=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],o=0,i=a;i!==r;)u[o++]=i.head,i=i.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),Na=function(t){return t.length};var Ry=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var Ny=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var CS=function(){function t(r,e,n,a,u,o){var i,m,s,c,D,b,f;for(i=u+(o-u>>1),i-u>1&&t(r,e,a,n,u,i),o-i>1&&t(r,e,a,n,i,o),m=u,s=i,c=u;m<i&&s<o;)D=a[m],b=a[s],f=e(r(D)(b)),f>0?(n[c++]=b,++s):(n[c++]=D,++m);for(;m<i;)n[c++]=a[m++];for(;s<o;)n[c++]=a[s++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var ac=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var Ly=function(t){return function(r){return t[r]}};var hS=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var By={defer:function(t){return function(r){return t(void 0)(r)}}},uc=function(t){return t.defer},$m=function(t){return function(r){var e=hS("go","Control.Lazy",function(){return uc(t)(function(a){return r(e(25))})}),n=e(25);return n}};function Cf(){return[]}var Mm=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var lp=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function TS(t){return function(){return t.slice()}}var _p=TS;var SS=function(){function t(r,e,n,a,u,o){var i,m,s,c,D,b,f;for(i=u+(o-u>>1),i-u>1&&t(r,e,a,n,u,i),o-i>1&&t(r,e,a,n,i,o),m=u,s=i,c=u;m<i&&s<o;)D=a[m],b=a[s],f=e(r(D)(b)),f>0?(n[c++]=b,++s):(n[c++]=D,++m);for(;m<i;)n[c++]=a[m++];for(;s<o;)n[c++]=a[s++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var Bi=function(t){return Mm([t])};var zy=function(t){return function(r){return t&&r}},Gy=function(t){return function(r){return t||r}},Vy=function(t){return!t};var Qa=function(t){return t.not};var Ui=function(t){return t.disj},La={ff:!1,tt:!0,implies:function(t){return function(r){return Ui(La)(Qa(La)(t))(r)}},conj:zy,disj:Gy,not:Vy};var jy=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},Xy=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var M=function(t){return t.empty};var ut=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),cc=function(t){return function(r){return t(r.value0)(r.value1)}};var tn=function(t){return t.value1};var ei={map:function(t){return function(r){return new ut(r.value0,t(r.value1))}}};var Ba=function(t){return t.value0};var qS=function(t){return{append:function(r){return function(e){return $u(t)(r)(e)}}}};var Il=function(t){return{mempty:Z(t),Semigroup0:function(){return qS(t.Semigroupoid0())}}};var Nn=function(){return _t};var rn=Nn,ke=Nn;var Nm=function(){return function(){return function(t){return Nn()}}};var re=function(t){return t.foldr};var Ce=function(t){return function(r){return re(t)(N(r.Alt0()))(M(r))}},_n=function(t){return function(r){return function(e){return re(t)(function(){var n=N(r.Alt0());return function(a){return n(e(a))}}())(M(r))}}},ae=function(t){return function(r){return function(e){return re(r)(function(){var n=X(t.Apply0());return function(a){return n(e(a))}}())(_(t)(void 0))}}},Ln=function(t){return function(r){return St(ae(t)(r))}},Dp=function(t){return function(r){return ae(t)(r)(Z(tt))}},me=function(t){return t.foldl};var Jr={foldr:function(t){return function(r){return function(e){if(e instanceof z)return r;if(e instanceof R)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof z)return r;if(e instanceof R)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof z)return $t(t);if(e instanceof R)return r(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,e.constructor.name])}}}};var tA=function(t){return function(r){return function(e){return re(t)(function(n){return function(a){return dt(r.Semigroup0())(e(n))(a)}})($t(r))}}},Nt={foldr:jy,foldl:Xy,foldMap:function(t){return tA(Nt)(t)}};var xn=function(t){return t.foldMap};var rA=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(o){return[a,u,o]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(o){return function(i){return function(m){function s(c,D){switch(D-c){case 0:return o([]);case 1:return u(t)(i(m[c]));case 2:return a(u(r)(i(m[c])))(i(m[c+1]));case 3:return a(a(u(e)(i(m[c])))(i(m[c+1])))(i(m[c+2]));default:var b=c+Math.floor((D-c)/4)*2;return a(u(n)(s(c,b)))(s(b,D))}}return s(0,m.length)}}}}}}();var Bn=function(t){return t.traverse};var lA=function(t){return function(r){return Bn(t)(r)(Z(tt))}},Fo={traverse:function(t){return rA(It(t.Apply0()))(p(t.Apply0().Functor0()))(_(t))},sequence:function(t){return lA(Fo)(t)},Functor0:function(){return Fr},Foldable1:function(){return Nt}};var Bl=function(){return ac(ut.create)}();var Qm=function(){return Ly};var dA=function(t){return[t]};var bA=function(){return Ry(R.create)(z.value)}();var Km=function(){return Ny(R.create)(z.value)}(),Ym=function(t){return function(r){return function(e){return e.length===0?[]:Zt(e)(function(n){return ra()(Km(n)(e))})(bA(t(r))(e))}}};var Hi=function(t){return function(r){return dt(hn)([t])(r)}};var yA=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var ro=function(t){return t.mapWithIndex};var ai={mapWithIndex:yA,Functor0:function(){return Fr}};var Oo=function(t){return t.foldrWithIndex};var eo=function(t){return t.foldlWithIndex};var ui=function(t){return t.foldMapWithIndex};var zi=function(t){return t.traverseWithIndex};var no=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Tp=function(t){return function(r){return new no(r,M(t))}};var Se=function(){function t(){}return t.value=new t,t}(),cr=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Sp=function(t){return t},eF=function(t){return new cr(t.value0,t.value1)};var nF=function(t){var r=function(e){return function(n){var a=e,u=!1,o;function i(m,s){if(s instanceof cr&&s.value1 instanceof cr&&s.value1.value1 instanceof cr){a=new cr(s,m),n=s.value1.value1.value1;return}var c=function(b){return b instanceof cr&&b.value1 instanceof cr&&b.value1.value1 instanceof Se?new cr(t(b.value0),new cr(t(b.value1.value0),Se.value)):b instanceof cr&&b.value1 instanceof Se?new cr(t(b.value0),Se.value):Se.value},D=function(b){return function(f){var h=b,et=!1,mt;function ur(jt,jr){if(jt instanceof cr&&jt.value0 instanceof cr&&jt.value0.value1 instanceof cr&&jt.value0.value1.value1 instanceof cr){h=jt.value1,f=new cr(t(jt.value0.value0),new cr(t(jt.value0.value1.value0),new cr(t(jt.value0.value1.value1.value0),jr)));return}return et=!0,jr}for(;!et;)mt=ur(h,f);return mt}};return u=!0,D(m)(c(s))}for(;!u;)o=i(a,n);return o}};return r(Se.value)},xp={map:nF};var Ua={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(o){var i=u,m=!1,s;function c(D,b){if(b instanceof Se)return m=!0,D;if(b instanceof cr){i=new cr(b.value0,D),o=b.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[D.constructor.name,b.constructor.name])}for(;!m;)s=c(i,o);return s}};return a(Se.value)}(),n=me(Ua)(St(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,o;function i(m,s){if(s instanceof Se)return u=!0,m;if(s instanceof cr){a=t(m)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)o=i(a,n);return o}};return r},foldMap:function(t){return function(r){return me(Ua)(function(e){var n=dt(t.Semigroup0())(e);return function(a){return n(r(a))}})($t(t))}}};var Ul={append:function(t){return function(r){return re(Ua)(cr.create)(r)(t)}}};var tv={append:function(t){return function(r){return new no(t.value0,dt(Ul)(t.value1)(eF(r)))}}};var gA={alt:dt(Ul),Functor0:function(){return xp}},rv=function(){return{empty:Se.value,Alt0:function(){return gA}}}();var xA=function(t){return t()};var FA=function(t){throw new Error(t)};var OA=function(){return FA};var EF=xA,Ka=function(t){return EF(function(){return OA()(t)})};var Kt=function(){function t(){}return t.value=new t,t}(),mr=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Ur=function(){function t(r,e,n,a,u,o,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o,this.value6=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return new t(r,e,n,a,u,o,i)}}}}}}},t}(),Vi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),fi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),Ji=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),wo=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),ji=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),Op=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var MA=function(t){return function(r){return new mr(Kt.value,t,r,Kt.value)}};var wA=function(t){return function(r){var e=rr(t),n=function(a){var u=!1,o;function i(m){if(m instanceof Kt)return u=!0,z.value;if(m instanceof mr){var s=e(r)(m.value1);if(s instanceof br)return u=!0,new R(m.value2);if(s instanceof tr){a=m.value0;return}a=m.value3;return}if(m instanceof Ur){var c=e(r)(m.value1);if(c instanceof br)return u=!0,new R(m.value2);var D=e(r)(m.value4);if(D instanceof br)return u=!0,new R(m.value5);if(c instanceof tr){a=m.value0;return}if(D instanceof dr){a=m.value6;return}a=m.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[m.constructor.name])}for(;!u;)o=i(a);return o};return n}};var PA=function(t){return t instanceof Kt};var en=function(t){return function(r){return function(e){var n=t,a=r,u=!1,o;function i(m,s,c){if(s instanceof Se)return u=!0,c;if(s instanceof cr){if(s.value0 instanceof Vi){n=m,a=s.value1,e=new mr(c,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof fi){n=m,a=s.value1,e=new mr(s.value0.value0,s.value0.value1,s.value0.value2,c);return}if(s.value0 instanceof Ji){n=m,a=s.value1,e=new Ur(c,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof wo){n=m,a=s.value1,e=new Ur(s.value0.value0,s.value0.value1,s.value0.value2,c,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof ji){n=m,a=s.value1,e=new Ur(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,c);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,c.constructor.name])}for(;!u;)o=i(n,a,e);return o}}},ql=function(t){return function(r){return function(e){var n=function(o){return function(i){var m=o,s=!1,c;function D(b,f){if(b instanceof Se)return s=!0,new mr(f.value0,f.value1,f.value2,f.value3);if(b instanceof cr){if(b.value0 instanceof Vi)return s=!0,en(t)(b.value1)(new Ur(f.value0,f.value1,f.value2,f.value3,b.value0.value0,b.value0.value1,b.value0.value2));if(b.value0 instanceof fi)return s=!0,en(t)(b.value1)(new Ur(b.value0.value0,b.value0.value1,b.value0.value2,f.value0,f.value1,f.value2,f.value3));if(b.value0 instanceof Ji){m=b.value1,i=new Op(new mr(f.value0,f.value1,f.value2,f.value3),b.value0.value0,b.value0.value1,new mr(b.value0.value2,b.value0.value3,b.value0.value4,b.value0.value5));return}if(b.value0 instanceof wo){m=b.value1,i=new Op(new mr(b.value0.value0,b.value0.value1,b.value0.value2,f.value0),f.value1,f.value2,new mr(f.value3,b.value0.value3,b.value0.value4,b.value0.value5));return}if(b.value0 instanceof ji){m=b.value1,i=new Op(new mr(b.value0.value0,b.value0.value1,b.value0.value2,b.value0.value3),b.value0.value4,b.value0.value5,new mr(f.value0,f.value1,f.value2,f.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[b.value0.constructor.name,f.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[b.constructor.name,f.constructor.name])}for(;!s;)c=D(m,i);return c}},a=rr(t),u=function(o){return function(i){var m=o,s=!1,c;function D(b,f){if(f instanceof Kt)return s=!0,n(b)(new Op(Kt.value,r,e,Kt.value));if(f instanceof mr){var h=a(r)(f.value1);if(h instanceof br)return s=!0,en(t)(b)(new mr(f.value0,r,e,f.value3));if(h instanceof tr){m=new cr(new Vi(f.value1,f.value2,f.value3),b),i=f.value0;return}m=new cr(new fi(f.value0,f.value1,f.value2),b),i=f.value3;return}if(f instanceof Ur){var et=a(r)(f.value1);if(et instanceof br)return s=!0,en(t)(b)(new Ur(f.value0,r,e,f.value3,f.value4,f.value5,f.value6));var mt=a(r)(f.value4);if(mt instanceof br)return s=!0,en(t)(b)(new Ur(f.value0,f.value1,f.value2,f.value3,r,e,f.value6));if(et instanceof tr){m=new cr(new Ji(f.value1,f.value2,f.value3,f.value4,f.value5,f.value6),b),i=f.value0;return}if(et instanceof dr&&mt instanceof tr){m=new cr(new wo(f.value0,f.value1,f.value2,f.value4,f.value5,f.value6),b),i=f.value3;return}m=new cr(new ji(f.value0,f.value1,f.value2,f.value3,f.value4,f.value5),b),i=f.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[b.constructor.name,f.constructor.name])}for(;!s;)c=D(m,i);return c}};return u(Se.value)}}},OF=function(t){return function(r){var e=function(i){return function(m){var s=i,c=!1,D;function b(f,h){if(f instanceof Se)return c=!0,h;if(f instanceof cr){if(f.value0 instanceof Vi&&f.value0.value2 instanceof Kt&&h instanceof Kt)return c=!0,en(t)(f.value1)(new mr(Kt.value,f.value0.value0,f.value0.value1,Kt.value));if(f.value0 instanceof fi&&f.value0.value0 instanceof Kt&&h instanceof Kt)return c=!0,en(t)(f.value1)(new mr(Kt.value,f.value0.value1,f.value0.value2,Kt.value));if(f.value0 instanceof Vi&&f.value0.value2 instanceof mr){s=f.value1,m=new Ur(h,f.value0.value0,f.value0.value1,f.value0.value2.value0,f.value0.value2.value1,f.value0.value2.value2,f.value0.value2.value3);return}if(f.value0 instanceof fi&&f.value0.value0 instanceof mr){s=f.value1,m=new Ur(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3,f.value0.value1,f.value0.value2,h);return}return f.value0 instanceof Vi&&f.value0.value2 instanceof Ur?(c=!0,en(t)(f.value1)(new mr(new mr(h,f.value0.value0,f.value0.value1,f.value0.value2.value0),f.value0.value2.value1,f.value0.value2.value2,new mr(f.value0.value2.value3,f.value0.value2.value4,f.value0.value2.value5,f.value0.value2.value6)))):f.value0 instanceof fi&&f.value0.value0 instanceof Ur?(c=!0,en(t)(f.value1)(new mr(new mr(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3),f.value0.value0.value4,f.value0.value0.value5,new mr(f.value0.value0.value6,f.value0.value1,f.value0.value2,h)))):f.value0 instanceof Ji&&f.value0.value2 instanceof Kt&&f.value0.value5 instanceof Kt&&h instanceof Kt?(c=!0,en(t)(f.value1)(new Ur(Kt.value,f.value0.value0,f.value0.value1,Kt.value,f.value0.value3,f.value0.value4,Kt.value))):f.value0 instanceof wo&&f.value0.value0 instanceof Kt&&f.value0.value5 instanceof Kt&&h instanceof Kt?(c=!0,en(t)(f.value1)(new Ur(Kt.value,f.value0.value1,f.value0.value2,Kt.value,f.value0.value3,f.value0.value4,Kt.value))):f.value0 instanceof ji&&f.value0.value0 instanceof Kt&&f.value0.value3 instanceof Kt&&h instanceof Kt?(c=!0,en(t)(f.value1)(new Ur(Kt.value,f.value0.value1,f.value0.value2,Kt.value,f.value0.value4,f.value0.value5,Kt.value))):f.value0 instanceof Ji&&f.value0.value2 instanceof mr?(c=!0,en(t)(f.value1)(new mr(new Ur(h,f.value0.value0,f.value0.value1,f.value0.value2.value0,f.value0.value2.value1,f.value0.value2.value2,f.value0.value2.value3),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof wo&&f.value0.value0 instanceof mr?(c=!0,en(t)(f.value1)(new mr(new Ur(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3,f.value0.value1,f.value0.value2,h),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof wo&&f.value0.value5 instanceof mr?(c=!0,en(t)(f.value1)(new mr(f.value0.value0,f.value0.value1,f.value0.value2,new Ur(h,f.value0.value3,f.value0.value4,f.value0.value5.value0,f.value0.value5.value1,f.value0.value5.value2,f.value0.value5.value3)))):f.value0 instanceof ji&&f.value0.value3 instanceof mr?(c=!0,en(t)(f.value1)(new mr(f.value0.value0,f.value0.value1,f.value0.value2,new Ur(f.value0.value3.value0,f.value0.value3.value1,f.value0.value3.value2,f.value0.value3.value3,f.value0.value4,f.value0.value5,h)))):f.value0 instanceof Ji&&f.value0.value2 instanceof Ur?(c=!0,en(t)(f.value1)(new Ur(new mr(h,f.value0.value0,f.value0.value1,f.value0.value2.value0),f.value0.value2.value1,f.value0.value2.value2,new mr(f.value0.value2.value3,f.value0.value2.value4,f.value0.value2.value5,f.value0.value2.value6),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof wo&&f.value0.value0 instanceof Ur?(c=!0,en(t)(f.value1)(new Ur(new mr(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3),f.value0.value0.value4,f.value0.value0.value5,new mr(f.value0.value0.value6,f.value0.value1,f.value0.value2,h),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof wo&&f.value0.value5 instanceof Ur?(c=!0,en(t)(f.value1)(new Ur(f.value0.value0,f.value0.value1,f.value0.value2,new mr(h,f.value0.value3,f.value0.value4,f.value0.value5.value0),f.value0.value5.value1,f.value0.value5.value2,new mr(f.value0.value5.value3,f.value0.value5.value4,f.value0.value5.value5,f.value0.value5.value6)))):f.value0 instanceof ji&&f.value0.value3 instanceof Ur?(c=!0,en(t)(f.value1)(new Ur(f.value0.value0,f.value0.value1,f.value0.value2,new mr(f.value0.value3.value0,f.value0.value3.value1,f.value0.value3.value2,f.value0.value3.value3),f.value0.value3.value4,f.value0.value3.value5,new mr(f.value0.value3.value6,f.value0.value4,f.value0.value5,h)))):(c=!0,Ka("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[f.constructor.name])}for(;!c;)D=b(s,m);return D}},n=function(i){return function(m){var s=i,c=!1,D;function b(f,h){if(h instanceof mr&&h.value0 instanceof Kt&&h.value3 instanceof Kt)return c=!0,e(f)(Kt.value);if(h instanceof mr){s=new cr(new fi(h.value0,h.value1,h.value2),f),m=h.value3;return}if(h instanceof Ur&&h.value0 instanceof Kt&&h.value3 instanceof Kt&&h.value6 instanceof Kt)return c=!0,e(new cr(new fi(Kt.value,h.value1,h.value2),f))(Kt.value);if(h instanceof Ur){s=new cr(new ji(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),f),m=h.value6;return}return c=!0,Ka("The impossible happened in partial function `removeMaxNode`.")}for(;!c;)D=b(s,m);return D}},a=function(i){var m=!1,s;function c(D){if(D instanceof mr&&D.value3 instanceof Kt)return m=!0,{key:D.value1,value:D.value2};if(D instanceof mr){i=D.value3;return}if(D instanceof Ur&&D.value6 instanceof Kt)return m=!0,{key:D.value4,value:D.value5};if(D instanceof Ur){i=D.value6;return}return m=!0,Ka("The impossible happened in partial function `maxNode`.")}for(;!m;)s=c(i);return s},u=rr(t),o=function(i){return function(m){var s=i,c=!1,D;function b(f,h){if(h instanceof Kt)return c=!0,z.value;if(h instanceof mr){var et=u(r)(h.value1);if(h.value3 instanceof Kt&&et instanceof br)return c=!0,new R(new ut(h.value2,e(f)(Kt.value)));if(et instanceof br){var mt=a(h.value0);return c=!0,new R(new ut(h.value2,n(new cr(new Vi(mt.key,mt.value,h.value3),f))(h.value0)))}if(et instanceof tr){s=new cr(new Vi(h.value1,h.value2,h.value3),f),m=h.value0;return}s=new cr(new fi(h.value0,h.value1,h.value2),f),m=h.value3;return}if(h instanceof Ur){var ur=function(){return h.value0 instanceof Kt&&h.value3 instanceof Kt&&h.value6 instanceof Kt}(),et=u(r)(h.value4),jt=u(r)(h.value1);if(ur&&jt instanceof br)return c=!0,new R(new ut(h.value2,en(t)(f)(new mr(Kt.value,h.value4,h.value5,Kt.value))));if(ur&&et instanceof br)return c=!0,new R(new ut(h.value5,en(t)(f)(new mr(Kt.value,h.value1,h.value2,Kt.value))));if(jt instanceof br){var mt=a(h.value0);return c=!0,new R(new ut(h.value2,n(new cr(new Ji(mt.key,mt.value,h.value3,h.value4,h.value5,h.value6),f))(h.value0)))}if(et instanceof br){var mt=a(h.value3);return c=!0,new R(new ut(h.value5,n(new cr(new wo(h.value0,h.value1,h.value2,mt.key,mt.value,h.value6),f))(h.value3)))}if(jt instanceof tr){s=new cr(new Ji(h.value1,h.value2,h.value3,h.value4,h.value5,h.value6),f),m=h.value0;return}if(jt instanceof dr&&et instanceof tr){s=new cr(new wo(h.value0,h.value1,h.value2,h.value4,h.value5,h.value6),f),m=h.value3;return}s=new cr(new ji(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),f),m=h.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[h.constructor.name])}for(;!c;)D=b(s,m);return D}};return o(Se.value)}},ha={foldr:function(t){return function(r){return function(e){if(e instanceof Kt)return r;if(e instanceof mr)return re(ha)(t)(t(e.value2)(re(ha)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return re(ha)(t)(t(e.value2)(re(ha)(t)(t(e.value5)(re(ha)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof Kt)return r;if(e instanceof mr)return me(ha)(t)(t(me(ha)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return me(ha)(t)(t(me(ha)(t)(t(me(ha)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof Kt)return $t(t);if(e instanceof mr)return dt(t.Semigroup0())(xn(ha)(t)(r)(e.value0))(dt(t.Semigroup0())(r(e.value2))(xn(ha)(t)(r)(e.value3)));if(e instanceof Ur)return dt(t.Semigroup0())(xn(ha)(t)(r)(e.value0))(dt(t.Semigroup0())(r(e.value2))(dt(t.Semigroup0())(xn(ha)(t)(r)(e.value3))(dt(t.Semigroup0())(r(e.value5))(xn(ha)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[e.constructor.name])}}}},la={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof Kt)return r;if(e instanceof mr)return Oo(la)(t)(t(e.value1)(e.value2)(Oo(la)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return Oo(la)(t)(t(e.value1)(e.value2)(Oo(la)(t)(t(e.value4)(e.value5)(Oo(la)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof Kt)return r;if(e instanceof mr)return eo(la)(t)(t(e.value1)(eo(la)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return eo(la)(t)(t(e.value4)(eo(la)(t)(t(e.value1)(eo(la)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){return function(r){return function(e){if(e instanceof Kt)return $t(t);if(e instanceof mr)return dt(t.Semigroup0())(ui(la)(t)(r)(e.value0))(dt(t.Semigroup0())(r(e.value1)(e.value2))(ui(la)(t)(r)(e.value3)));if(e instanceof Ur)return dt(t.Semigroup0())(ui(la)(t)(r)(e.value0))(dt(t.Semigroup0())(r(e.value1)(e.value2))(dt(t.Semigroup0())(ui(la)(t)(r)(e.value3))(dt(t.Semigroup0())(r(e.value4)(e.value5))(ui(la)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[e.constructor.name])}}},Foldable0:function(){return ha}},IA=function(){return Oo(la)(function(t){return function(r){return function(e){return new cr(t,e)}}})(Se.value)}();var Hl=function(){return Kt.value}();var ov=function(t){return function(r){return function(e){return Zt(e)(tn)(OF(t)(r)(e))}}};var zl=function(t){return function(r){return function(e){return function(n){var a=r(wA(t)(e)(n));if(a instanceof z)return ov(t)(e)(n);if(a instanceof R)return ql(t)(e)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var $F=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(o){return function(i){return zl(t)(function(){var m=Zt(i)(r(i));return function(s){return R.create(m(s))}}())(u)(o)}}};return eo(la)(a)(n)(e)}}}};var RA=function(t){return $F(t)(x)};var Gl=function(t){return t.partitionMap};var Xi=function(t){return t.filterMap};var Vl=function(t){return t.filter};var Ac=function(t){return{always:Z(tt),Monoid0:function(){return t}}};var Un={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},Po=function(t){return t.dimap},Ya=function(t){return function(r){return Po(t)(r)(Z(tt))}};var RF=function(t){return function(r){return function(e){return RA(t)(r)(e)}}};var fv=function(t){return IA(t)};var HA=function(t){return MA(t)(void 0)};var cv=function(t){return{append:RF(t)}};var zA=function(t){return PA(t)},GA=function(t){return function(r){return function(e){return ql(t)(r)(void 0)(e)}}};var VA={foldMap:function(t){return function(r){var e=xn(Ua)(t)(r);return function(n){return e(fv(n))}}},foldl:function(t){return function(r){var e=me(Ua)(t)(r);return function(n){return e(fv(n))}}},foldr:function(t){return function(r){var e=re(Ua)(t)(r);return function(n){return e(fv(n))}}}};var lv=Hl;var JA=function(t){return{mempty:lv,Semigroup0:function(){return cv(t)}}};var Pp=function(t){return function(r){return function(e){return ov(t)(r)(e)}}};function jA(t){return function(r){return function(){return setTimeout(r,t)}}}function XA(t){return function(){clearTimeout(t)}}var Ip=jA;var LF={eq:function(t){return function(r){return t===r}}},Rp={compare:function(t){return function(r){return rr(Je)(t)(r)}},Eq0:function(){return LF}};var jl=XA;var ci=function(t){return t.sampleOn};var xe=function(t){return t.keepLatest};var Pu=function(t){return t.fold};var Xl=function(t){return function(r){return function(e){return function(n){return Xi(t.Filterable1())(tn)(Pu(t)(function(a){return function(u){return p(ei)(_(Eo))(r(a)(u.value0))}})(e)(new ut(n,z.value)))}}}},Np=function(t){return function(r){var e=function(n){return function(a){if(a instanceof z)return new R({now:n,last:z.value});if(a instanceof R)return new R({now:n,last:new R(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 59, column 3 - line 59, column 50): "+[n.constructor.name,a.constructor.name])}};return Xi(t.Filterable1())(Z(tt))(Pu(t)(e)(r)(z.value))}},Ql=function(t){return t.fix};var Fn=function(t){return function(r){return function(e){return It(t.Alternative0().Applicative0().Apply0())(p(t.Filterable1().Functor1())(Ri)(r))(e)}}};function pv(t){return function(r){return t===r}}var sv=pv;var qF=function(t){return t};var xt=function(t){return function(r){return t(r)}},HF=function(t){return function(r){return function(e){return function(n){return function(a){return W(t.Monad0().Bind1())(Rt(t)(En(z.value)))(function(u){return W(t.Monad0().Bind1())(e(function(o){return Rt(t)(yr(To)(Rn(new R(o))(u)))}))(function(o){return W(t.Monad0().Bind1())(n(function(i){return W(t.Monad0().Bind1())(Rt(t)(je(u)))(ae(r)(Jr)(function(m){return a(i(m))}))}))(function(i){return _(r)(X(r.Apply0())(o)(i))})})})}}}}},qt=qF;var zF=function(t){return function(r){return function(e){return W(t.Monad0().Bind1())(Rt(t)(En(z.value)))(function(n){return W(t.Monad0().Bind1())(r(function(a){return te(Gr)(t.Monad0().Bind1())(W(t.Monad0().Bind1())(Rt(t)(je(n)))(Dp(t.Monad0().Applicative0())(Jr)))(function(){return W(t.Monad0().Bind1())(xt(a)(e))(function(u){return Rt(t)(yr(To)(Rn(new R(u))(n)))})})}))(function(a){return _(t.Monad0().Applicative0())(te(Gr)(t.Monad0().Bind1())(W(t.Monad0().Bind1())(Rt(t)(je(n)))(Dp(t.Monad0().Applicative0())(Jr)))(function(){return a}))})})}}},C={map:function(t){return function(r){return function(e){return r(function(n){return e(t(n))})}}}};var GF=function(t){return function(r){return function(e){return function(n){return function(a){return W(t.Monad0().Bind1())(Rt(t)(En(n)))(function(u){return e(function(o){return W(t.Monad0().Bind1())(Rt(t)(Mu(r(o))(u)))(a)})})}}}}},Kl=function(t){return function(r){return function(e){return function(n){return e(function(a){var u=r(a);if(u instanceof R)return n(u.value0);if(u instanceof z)return _(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 159, column 13 - line 161, column 27): "+[u.constructor.name])})}}}},mv=function(t){return function(r){return Kl(t)(function(e){var n=r(e);if(n)return new R(e);if(!n)return z.value;throw new Error("Failed pattern match at FRP.Event (line 93, column 13 - line 95, column 25): "+[n.constructor.name])})}},Iu=function(t){return function(r){return qt(function(e){return function(){var a=pe($t(JA(Rp)))(),u=xt(r)(function(o){return function(){var m=pe(z.value)(),s=Ip(t)(function(){e(o)();var D=Pe(m)();return Zt(_(l)(void 0))(function(b){return gf(Pp(Rp)(b))(a)})(D)()})();return Sn(new R(s))(m)(),gf(dt(cv(Rp))(HA(s)))(a)()}})();return function(){var i=Pe(a)();return Ln(l)(VA)(i)(jl)(),u()}}})}};var Yl=function(t){return function(r){return W(t.Monad0().Bind1())(Rt(t)(En([])))(function(e){return _(t.Monad0().Applicative0())({event:function(n){return W(r.Monad0().Bind1())(Rt(r)(Mu(function(a){return dt(hn)(a)([n])})(e)))(function(){return _(r.Monad0().Applicative0())(W(r.Monad0().Bind1())(Rt(r)(Mu(Ym(sv)(n))(e)))(function(){return _(r.Monad0().Applicative0())(void 0)}))})},push:function(n){return W(r.Monad0().Bind1())(Rt(r)(je(e)))(ae(r.Monad0().Applicative0())(Nt)(function(a){return a(n)}))}})})}},VF=function(t){return function(r){return function(e){return function(n){return W(r.Bind1())(Yl(t)(t))(function(a){var u=e(a.event);return W(r.Bind1())(xt(u.input)(a.push))(function(o){return W(r.Bind1())(xt(u.output)(n))(function(i){return _(r.Applicative0())(X(r.Bind1().Apply0())(o)(i))})})})}}}};var Qi=function(t){return function(r){return function(e){return qt(function(n){return W(t.Monad0().Bind1())(Yl(t)(t))(function(a){return te(Gr)(t.Monad0().Bind1())(n(e(a.event)))(function(){return xt(r)(a.push)})})})}}},YA=function(t){return{compact:Kl(t)(Z(tt)),separate:function(r){return{left:Kl(t)(function(e){if(e instanceof Xt)return new R(e.value0);if(e instanceof Qt)return z.value;throw new Error("Failed pattern match at FRP.Event (line 76, column 13 - line 78, column 33): "+[e.constructor.name])})(r),right:Kl(t)(function(e){if(e instanceof Qt)return new R(e.value0);if(e instanceof Xt)return z.value;throw new Error("Failed pattern match at FRP.Event (line 83, column 13 - line 85, column 32): "+[e.constructor.name])})(r)}}}},du=function(t){return{filter:mv(t),filterMap:Kl(t),partition:function(r){return function(e){return{yes:mv(t)(r)(e),no:mv(t)(function(){var n=Qa(La);return function(a){return n(r(a))}}())(e)}}},partitionMap:function(r){return function(e){return{left:Xi(du(t))(function(){var n=Ia(R.create)(x(z.value));return function(a){return n(r(a))}}())(e),right:Xi(du(t))(function(n){return fp(r(n))})(e)}}},Compactable0:function(){return YA(t)},Functor1:function(){return C}}},Be=function(t){return function(r){return qt(function(e){return W(t.Monad0().Bind1())(Yl(t)(t))(function(n){return te(Gr)(t.Monad0().Bind1())(e(r(n.push)(n.event)))(function(){return _(t.Monad0().Applicative0())(_(t.Monad0().Applicative0())(void 0))})})})}},JF=function(t){return function(r){return function(e){return function(n){return function(a){return W(t.Monad0().Bind1())(Rt(t)(En(z.value)))(function(u){return W(t.Monad0().Bind1())(Rt(t)(Cf))(function(o){return W(t.Monad0().Bind1())(Rt(t)(En(z.value)))(function(i){return W(t.Monad0().Bind1())(Rt(t)(Cf))(function(m){return W(t.Monad0().Bind1())(Rt(t)(En(!0)))(function(s){return W(t.Monad0().Bind1())(e(function(c){return W(t.Monad0().Bind1())(Rt(t)(je(s)))(function(D){return D?Rt(t)(yr(To)(Bi(c)(o))):W(t.Monad0().Bind1())(Rt(t)(Rn(new R(c))(u)))(function(){return W(t.Monad0().Bind1())(Rt(t)(je(i)))(ae(r)(Jr)(function(b){return a(b(c))}))})})}))(function(c){return W(t.Monad0().Bind1())(n(function(D){return W(t.Monad0().Bind1())(Rt(t)(je(s)))(function(b){return b?Rt(t)(yr(To)(Bi(D)(m))):W(t.Monad0().Bind1())(Rt(t)(Rn(new R(D))(i)))(function(){return W(t.Monad0().Bind1())(Rt(t)(je(u)))(ae(r)(Jr)(function(f){return a(D(f))}))})})}))(function(D){return W(t.Monad0().Bind1())(Rt(t)(Rn(!1)(s)))(function(){return W(t.Monad0().Bind1())(Rt(t)(_p(o)))(function(b){return W(t.Monad0().Bind1())(Rt(t)(_p(m)))(function(f){return te(Gr)(t.Monad0().Bind1())(Ln(r)(Nt)(b)(function(h){return W(t.Monad0().Bind1())(Rt(t)(Rn(new R(h))(u)))(function(){return Ln(r)(Nt)(f)(function(et){return W(t.Monad0().Bind1())(Rt(t)(Rn(new R(et))(i)))(function(){return a(et(h))})})})}))(function(){return W(t.Monad0().Bind1())(Rt(t)(lp(0)(Na(b))([])(o)))(function(){return W(t.Monad0().Bind1())(Rt(t)(lp(0)(Na(f))([])(m)))(function(){return _(r)(X(r.Apply0())(c)(D))})})})})})})})})})})})})})}}}}},jF=function(t){return{apply:function(r){return function(e){return JF(t)(t.Monad0().Applicative0())(r)(p(C)(Ri)(e))}},Functor0:function(){return C}}};var E=function(t){return{pure:function(r){return function(e){return Zf(t.Monad0().Bind1().Apply0().Functor0())(_(t.Monad0().Applicative0())(void 0))(e(r))}},Apply0:function(){return jF(t)}}};var L=function(t){return{alt:function(r){return function(e){return function(n){return It(t.Apply0())(p(t.Apply0().Functor0())(function(a){return function(u){return X(t.Apply0())(a)(u)}})(r(n)))(e(n))}}},Functor0:function(){return C}}},S=function(t){return{empty:function(r){return _(t)(_(t)(void 0))},Alt0:function(){return L(t)}}},XF=function(t){return{Applicative0:function(){return E(t)},Plus1:function(){return S(t.Monad0().Applicative0())}}},Lt=function(t){return{fold:GF(t),keepLatest:zF(t),sampleOn:HF(t)(t.Monad0().Applicative0()),fix:VF(t)(t.Monad0()),Alternative0:function(){return XF(t)},Filterable1:function(){return du(t.Monad0().Applicative0())}}};var Lp="_____$__$_$$_vbus";function vv(t){return t[Lp]=Lp,t}function Dv(t){return()=>{for(let r in t)delete t[r]}}function dv(t){return()=>{let r=(u,o,i,m)=>{let s=Object.keys(m);for(var c=0;c<s.length;c++)if(m[s[c]]instanceof Object&&m[s[c]][Lp]===Lp){let D={},b={};r(u,D,b,m[s[c]]),o[s[c]]=D,i[s[c]]=b}else{let D=`${Math.random()}`;u[D]={},o[s[c]]=b=>()=>{let f=Object.values(u[D]);for(var h=0;h<f.length;h++)f[h](b)()},i[s[c]]=b=>()=>{let f=`${Math.random()}`;return u[D][f]=b,()=>{u[D]&&u[D][f]&&delete u[D][f]}}}},e={},n={},a={};return r(e,n,a,t),{p:n,e:a,s:e}}}function Zl(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var tk=function(t){return function(){return function(){return function(r){return function(e){return function(n){return Gu(be(t)(r))(e)(n)}}}}}};var rk=function(){return function(){return function(t){return function(r){return Zl(t,r)}}}},t_=function(t){return function(){return function(){return function(r){return function(e){return function(n){return Gu(be(t)(r))(e)(n)}}}}}},li=function(t){return function(){return function(r){return function(e){return wa(be(t)(r))(e)}}}};var Wn={vb:function(t){return function(r){return function(e){return{}}}}},Bp=function(t){return t.vb},tu=function(){return function(t){return function(r){return function(e){return function(n){var a=Bp(r)(y.value)(y.value)(y.value);return qt(function(u){return W(t.Monad0().Bind1())(dv(a))(function(o){return te(Gr)(t.Monad0().Bind1())(u(n(o.p)(o.e)))(function(){return _(t.Monad0().Applicative0())(Dv(o.s))})})})}}}}},Ru=function(t){return function(){return function(){return function(){return function(r){return function(e){return function(){return function(){return function(){return function(){return{vb:function(n){return function(a){return function(u){return t_(t)()()(y.value)(vv(Bp(r)(y.value)(y.value)(y.value)))(Bp(e)(y.value)(y.value)(y.value))}}}}}}}}}}}}}},ue=function(t){return function(){return function(){return function(r){return function(){return function(){return function(){return function(){return{vb:function(e){return function(n){return function(a){return t_(t)()()(y.value)(void 0)(Bp(r)(y.value)(y.value)(y.value))}}}}}}}}}}}};var Zi=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),yv=function(){function t(){}return t.value=new t,t}();var kc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),gc=function(){function t(){}return t.value=new t,t}(),Av=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Up=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Tf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Sf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),P=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var ek=function(t){return t};var Wp={eq:function(t){return function(r){return t instanceof Zi&&r instanceof Zi?t.value0===r.value0:t instanceof yv&&r instanceof yv}}};var q=function(t){return new Tf(t)},rt=function(t){return new Sf(t)},qp=function(t){return new Up(t)};var nk=t=>r=>r[t];var Io=function(t){return t.reflectType};var uk=function(t){return Io(t)};var r_=Fr;var xf=function(){return function(t){return t}};var ck=function(t){return[t]};var lk=function(){return function(){return function(){return function(){return function(){return function(t){var r=uk(t);return function(e){return nk(r(e))}}}}}}};var kv=[];var Ff=function(){return function(){return function(t){return function(r){return Hi(t)(r)}}}};function _k(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var Ro={};function gv(t){return t()}function pk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function sk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function mk(t){return function(r){return function(e){return function(n){var a=e;function u(i){return function(m){return r(m)(i)(n[i])}}for(var o in n)hasOwnProperty.call(n,o)&&(a=t(a)(u(o)));return a}}}}function e_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var uO=Object.keys||e_(function(t){return function(){return t}});function Cv(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var Ev=function(t){return function(r){return function(){return delete r[t],r}}};var hv=e_(function(t){return function(r){return r}});var pO=_k;var Dk=function(t){return function(r){return gv(function(){var n=pO(r)();return t(n)(),n})}};var dk=function(t){return function(r){return sk(r,t)}};var pi=function(t){return function(r){return Dk(Cv(t)(r))}},Gp={map:function(t){return function(r){return pk(r,t)}}},sO={mapWithIndex:dk,Functor0:function(){return Gp}},Tv=function(){return _t};var Vp=mk(Ri),bk=function(t){return function(r){return Vp(function(e){return function(n){return function(a){return dt(t.Semigroup0())(e)(r(n)(a))}}})($t(t))}},n_={foldl:function(t){return Vp(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return re(Nt)(t)(r)(hv(e))}}},foldMap:function(t){return function(r){return bk(t)(x(r))}}},yk={foldlWithIndex:function(t){return Vp(St(t))},foldrWithIndex:function(t){return function(r){return function(e){return re(Nt)(cc(t))(r)(e_(ut.create)(e))}}},foldMapWithIndex:function(t){return bk(t)},Foldable0:function(){return n_}},mO={traverseWithIndex:function(t){return function(r){return function(e){return Vp(function(n){return function(a){return function(u){return It(t.Apply0())(p(t.Apply0().Functor0())(St(pi(a)))(n))(r(a)(u))}}})(_(t)(Ro))(e)}}},FunctorWithIndex0:function(){return sO},FoldableWithIndex1:function(){return yk},Traversable2:function(){return Cc}},Cc={traverse:function(t){var r=zi(mO)(t);return function(e){return r(x(e))}},sequence:function(t){return Bn(Cc)(t)(Z(tt))},Functor0:function(){return Gp},Foldable1:function(){return n_}};var Sv=function(t){return Dk(Ev(t))};var Ak=function(){function t(){}return t.value=new t,t}(),xv=function(){function t(){}return t.value=new t,t}(),vO=function(){function t(){}return t.value=new t,t}();var Fv=function(t){return function(r){return function(e){var n=function(a){var u=function(o){return function(i){return new ut(i+1|0,new ut(o,i))}};return Xl(Lt(t))(u)(a)(0)};return new Up(xe(Lt(t))(Qi(t)(n(e))(function(a){return p(C)(function(u){return N(L(t.Monad0().Applicative0()))(_(E(t))(new kc(r(u.value0))))(p(C)(x(gc.value))(Vl(du(t.Monad0().Applicative0()))(function(){var o=ar(Ii)(u.value1+1|0);return function(i){return o(tn(i))}}())(a)))})(a)})))}}};var ao=function(t){return function(r){return function(e){return function(n){return function(a){var u=function(o){return o(n)(a)};return function(o){if(o instanceof Tf)return _n(Nt)(S(t))(ao(t)(r)(e)(n)(a))(o.value0);if(o instanceof Sf)return xe(Lt(r))(p(C)(ao(t)(r)(e)(n)(a))(o.value0));if(o instanceof P)return u(e.toElt(o.value0));if(o instanceof Up)return qt(function(i){return W(r.Monad0().Bind1())(Rt(r)(En(Ro)))(function(m){return W(r.Monad0().Bind1())(xt(o.value0)(function(s){return W(r.Monad0().Bind1())(e.ids(a))(function(c){return W(r.Monad0().Bind1())(Rt(r)(En(_(t)(void 0))))(function(D){return W(r.Monad0().Bind1())(e.ids(a))(function(b){return W(r.Monad0().Bind1())(Rt(r)(En(_(t)(void 0))))(function(f){return W(r.Monad0().Bind1())(Rt(r)(En([])))(function(h){return W(r.Monad0().Bind1())(Rt(r)(En(_(t)(void 0))))(function(et){return W(r.Monad0().Bind1())(p(t.Apply0().Functor0())(Zi.create)(e.ids(a)))(function(mt){return W(r.Monad0().Bind1())(Rt(r)(En(Ak.value)))(function(ur){return W(r.Monad0().Bind1())(xt(s)(function(jt){return W(r.Monad0().Bind1())(Rt(r)(je(ur)))(function(jr){return jt instanceof Av&&jr instanceof xv?W(r.Monad0().Bind1())(Rt(r)(je(h)))(ae(t)(Nt)(function(){var Qr=e.doLogic(jt.value0)(a);return function(ct){return i(Qr(ct))}}())):jt instanceof gc&&jr instanceof xv?te(Gr)(r.Monad0().Bind1())(yr(t.Apply0().Functor0())(Rt(r)(Rn(vO.value)(ur))))(function(){var Qr=X(t.Apply0())(X(t.Apply0())(X(t.Apply0())(X(t.Apply0())(W(r.Monad0().Bind1())(Rt(r)(je(h)))(ae(t)(Nt)(function(ct){return Ln(t)(Jr)(n.parent)(function(_r){return i(e.disconnectElement(a)({id:ct,parent:_r,scope:mt}))})})))(Va(r.Monad0().Bind1())(Rt(r)(je(D)))))(Va(r.Monad0().Bind1())(Rt(r)(je(f)))))(yr(t.Apply0().Functor0())(Rt(r)(Mu(Sv(c))(m)))))(yr(t.Apply0().Functor0())(Rt(r)(Mu(Sv(b))(m))));return X(t.Apply0())(yr(t.Apply0().Functor0())(Rt(r)(Rn(Qr)(et))))(Qr)}):jt instanceof kc&&jr instanceof Ak?te(Gr)(r.Monad0().Bind1())(yr(t.Apply0().Functor0())(Rt(r)(Rn(xv.value)(ur))))(function(){return W(r.Monad0().Bind1())(xt(ao(t)(r)(e)(function(){var Qr={};for(var ct in n)({}).hasOwnProperty.call(n,ct)&&(Qr[ct]=n[ct]);return Qr.scope=mt,Qr.raiseId=function(_r){return yr(t.Apply0().Functor0())(Rt(r)(Mu(dt(hn)([_r]))(h)))},Qr}())(a)(jt.value0))(i))(function(Qr){return te(Gr)(r.Monad0().Bind1())(yr(t.Apply0().Functor0())(Rt(r)(Mu(pi(b)(Qr))(m))))(function(){return yr(t.Apply0().Functor0())(Rt(r)(Rn(Qr)(f)))})})}):_(t)(void 0)})}))(function(jt){return te(Gr)(r.Monad0().Bind1())(yr(t.Apply0().Functor0())(Rt(r)(Rn(jt)(D))))(function(){return te(Gr)(r.Monad0().Bind1())(yr(t.Apply0().Functor0())(Rt(r)(Mu(pi(c)(jt))(m))))(function(){return Va(r.Monad0().Bind1())(Rt(r)(je(et)))})})})})})})})})})})})}))(function(s){return _(t)(te(Gr)(r.Monad0().Bind1())(W(r.Monad0().Bind1())(Rt(r)(je(m)))(me(n_)(X(t.Apply0()))(_(t)(void 0))))(function(){return s}))})})});throw new Error("Failed pattern match at Bolson.Control (line 530, column 17 - line 614, column 20): "+[o.constructor.name])}}}}}},DO=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){var i=function(m){return function(s){return qt(function(c){return W(t.Monad0().Bind1())(jb(p(Fr)(x(""))(xf()(u))))(function(D){var b=Ce(Nt)(S(t.Monad0().Applicative0()))(ro(ai)(function(f){return $m(By)(function(h){return function(et){return et instanceof P?function(mt){return mt(function(){var ur={};for(var jt in m)({}).hasOwnProperty.call(m,jt)&&(ur[jt]=m[jt]);return ur.parent=z.value,ur.scope=e(m.scope),ur.raiseId=function(jr){return Xb(f)(jr)(D)},ur}())(s)}(a.toElt(et.value0)):h(a.wrapElt(et))}})})(xf()(u)));return W(t.Monad0().Bind1())(xt(b)(c))(function(f){return W(t.Monad0().Bind1())(Rt(t)(En(_(t.Monad0().Applicative0())(void 0))))(function(h){return W(t.Monad0().Bind1())(p(t.Monad0().Bind1().Apply0().Functor0())(_t)(Qb(D)))(function(et){var mt=p(r_)(function(jt){return function(jr){return new P(a.fromEltO1(function(Qr){return function(ct){return qt(function(_r){return te(Gr)(t.Monad0().Bind1())(Qr.raiseId(jt))(function(){return te(Gr)(t.Monad0().Bind1())(Ln(t.Monad0().Applicative0())(Jr)(Qr.parent)(function(hr){return _r(a.giveNewParent(ct)({id:jt,parent:hr,scope:Qr.scope})(jr))}))(function(){return _(t.Monad0().Applicative0())(_(t.Monad0().Applicative0())(void 0))})})})}}))}})(et),ur=ao(t.Monad0().Applicative0())(t)(n)(m)(s)(o(mt)(_t));return W(t.Monad0().Bind1())(xt(ur)(c))(function(jt){return te(Gr)(t.Monad0().Bind1())(yr(t.Monad0().Bind1().Apply0().Functor0())(Rt(t)(Rn(jt)(h))))(function(){return _(t.Monad0().Applicative0())(te(Gr)(t.Monad0().Bind1())(f)(function(){return te(Gr)(t.Monad0().Bind1())(In(t.Monad0().Applicative0())(!r)(Ln(t.Monad0().Applicative0())(Nt)(xf()(et))(function(jr){return c(a.deleteFromCache(s)({id:jr}))})))(function(){return Va(t.Monad0().Bind1())(Rt(t)(je(h)))})}))})})})})})})})}};return new P(a.fromEltO2(i))}}}}}}}};var Ov=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return DO()(t)(!1)(Z(tt))(r)(e)(n)(a)}}}}}};var kk=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(o){return qt(function(i){return W(t.Monad0().Bind1())(Rt(t)(En(z.value)))(function(m){var s=n(new P(e.fromElt(function(c){return function(D){return qt(function(b){return te(Gr)(t.Monad0().Bind1())(W(t.Monad0().Bind1())(Rt(t)(je(m)))(function(f){if(f instanceof z)return _(t.Monad0().Applicative0())(void 0);if(f instanceof R)return Ln(t.Monad0().Applicative0())(Jr)(c.parent)(function(h){return In(t.Monad0().Applicative0())(f.value0!==h)(X(t.Monad0().Bind1().Apply0())(c.raiseId(f.value0))(b(e.connectToParent(o)({id:f.value0,parent:h}))))});throw new Error("Failed pattern match at Bolson.Control (line 640, column 36 - line 647, column 16): "+[f.constructor.name])}))(function(){return _(t.Monad0().Applicative0())(_(t.Monad0().Applicative0())(void 0))})})}})));return xt(ao(t.Monad0().Applicative0())(t)(r)(function(){var c={};for(var D in u)({}).hasOwnProperty.call(u,D)&&(c[D]=u[D]);return c.parent=u.parent,c.scope=u.scope,c.raiseId=function(b){return te(Gr)(t.Monad0().Bind1())(u.raiseId(b))(function(){return yr(t.Monad0().Bind1().Apply0().Functor0())(Rt(t)(Rn(new R(b))(m)))})},c}())(o)(s))(i)})})}};return new P(e.fromElt(a))}}}};var dO=function(t){return t},a_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),u_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),bO=function(t){return t},jp=Nn(),A=bO;var H=function(){return a_.create}();var lt=function(){return u_.create}(),Zr=function(){var t=p(ca)(p(F)(x(!0)));return function(r){return dO(t(r))}}(),Y=function(t){return t.attr};var AO=function(t){return t.makeText},kO=function(t){return function(r){return function(e){return p(C)(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},gO=function(t){return function(r){return function(e){return p(C)(function(n){return function(a){if(a.value instanceof a_)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof u_)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 83, column 26 - line 85, column 45): "+[a.value.constructor.name])}(jp(n))})(e)}}},CO=function(t){return t.makeElement},EO=function(t){return t.attributeParent},nn=function(t){return function(r){var e=function(n){return function(a){return qt(function(u){return W(t.Monad0().Bind1())(a.ids)(function(o){return te(Gr)(t.Monad0().Bind1())(n.raiseId(o))(function(){return p(t.Monad0().Bind1().Apply0().Functor0())(X(t.Monad0().Bind1().Apply0())(u(a.deleteFromCache({id:o}))))(xt(Ce(Nt)(S(t.Monad0().Applicative0()))([_(E(t))(AO(a)({id:o,parent:n.parent,scope:n.scope})),kO(a)(o)(r)]))(u))})})})}};return new P(e)}},oe=function(t){return function(r){return nn(t)(_(E(t))(r))}},hO=function(t){return{doLogic:function(r){return function(e){return function(n){return e.sendToPos({id:n,pos:r})}}},ids:function(){var r=ke(t);return function(e){return function(n){return n.ids}(r(e))}}(),disconnectElement:function(r){return function(e){return r.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:ar(Wp)})}},toElt:function(r){return r}}};var gk=function(t){return ao(t.MonadST5().Monad0().Applicative0())(t.MonadST5())(hO())},TO=function(t){return function(r){return function(e){return function(n){return qt(function(a){return W(t.MonadST5().Monad0().Bind1())(n.ids)(function(u){return xt(N(L(t.MonadST5().Monad0().Applicative0()))(_(E(t.MonadST5()))(n.makeRoot({id:u,root:r})))(gk(t)({parent:new R(u),scope:new Zi("rootScope"),raiseId:function(o){return _(t.MonadST5().Monad0().Applicative0())(void 0)},pos:z.value})(n)(e)))(a)})})}}}};var Ck=function(t){return function(r){return function(e){return TO(t)(r)(new Tf(e))}}},j=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(o){return qt(function(i){return W(t.MonadST5().Monad0().Bind1())(o.ids)(function(m){return te(Gr)(t.MonadST5().Monad0().Bind1())(u.raiseId(m))(function(){return p(t.MonadST5().Monad0().Bind1().Apply0().Functor0())(function(s){return X(t.MonadST5().Monad0().Bind1().Apply0())(i(o.deleteFromCache({id:m})))(s)})(xt(N(L(t.MonadST5().Monad0().Applicative0()))(Ce(Nt)(S(t.MonadST5().Monad0().Applicative0()))(dt(hn)([_(E(t.MonadST5()))(CO(o)({id:m,parent:u.parent,scope:u.scope,tag:r})),gO(o)(m)(e)])(Zt([])(function(s){return[_(E(t.MonadST5()))(EO(o)({id:m,parent:s,pos:u.pos}))]})(u.parent))))(gk(t)({parent:new R(m),scope:u.scope,raiseId:function(s){return _(t.MonadST5().Monad0().Applicative0())(void 0)},pos:z.value})(o)(n)))(i))})})})}};return a}}}};var PO=function(){return function(){return function(){return function(t){return function(r){return function(e){return bl(e.type)(t)?wa(e.type)(t)(e.value):r(e)}}}}}};var ie=function(){return function(t){return function(r){return function(e){return{type:be(t)(r),value:e}}}}};var Sk=function(t){return Ka("Data.Variant: pattern match failure ["+(t.type+"]"))},Ue=function(){return function(){return function(){return function(t){return PO()()()(t)(Sk)}}}};function xk(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function Fk(t){return function(r){return function(e){return e[t]=r,e}}}var Xp=Zo;var Mv=function(){return function(){return function(t){return function(r){return function(e){return function(n){return Fk(be(t)(r))(e)(n)}}}}}};var Qp=tt,wv=function(t){return function(r){return t(xk(r))}},Ok=St(wv)({});var v={Always0:function(){return Ac(Ae(we))},Always1:function(){return Ac(Ae(Ae(we)))},Always2:function(){return Ac(Ae(we))},Always3:function(){return Ac(Il(tt))},Always4:function(){return Ac(Il(tt))},MonadST5:function(){return d}};var se=function(){function t(){}return t.value=new t,t}();var ve={attr:function(t){return function(r){return A({key:"click",value:lt(r)})}}};var Vt=function(){function t(){}return t.value=new t,t}();var Kp={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}};var Mk={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}};var st={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}};var wk={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}},Ec={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}};var Iv={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}};var Pk={attr:function(t){return function(r){return A({key:"style",value:H(r)})}}};var Rv=function(t){return function(r){return function(e){return new P(j(t)("a")(r)(q(e)))}}};var Sr=function(t){return function(r){return function(e){return new P(j(t)("div")(r)(q(e)))}}},Wr=function(t){return Sr(t)(M(S(t.MonadST5().Monad0().Applicative0())))};var Tc=function(t){return function(r){return function(e){return new P(j(t)("span")(r)(q(e)))}}},Nv=function(t){return Tc(t)(M(S(t.MonadST5().Monad0().Applicative0())))};var Rk=(t,r,e,n)=>{t(a=>n.units[a].main.appendChild(n.units[r].main))(e)};var Nk=t=>r=>()=>{r.units[t.id].main.parentNode||(typeof t.pos.value0=="number"&&r.units[t.parent].main.children[t.pos.value0]?r.units[t.parent].main.insertBefore(r.units[t.id].main,r.units[t.parent].main.children[t.pos.value0]):r.units[t.parent].main.appendChild(r.units[t.id].main))},Lk=t=>r=>e=>()=>{var n,a=r.id;e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(a),e.hydrating&&t&&r.parent.value0&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:n}:e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:document.createElement(r.tag)}},Bk=t=>r=>e=>n=>()=>{var a=e.id,u;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(a),n.hydrating&&t&&e.parent.value0&&(u=document.body.querySelectorAll("[data-deku-ssr-"+e.parent.value0+"]").item(0))){var o=0;if(u.childNodes.length===1)u.prepend(document.createTextNode(""));else for(var o=0;o<u.childNodes.length;o++)if(u.childNodes[o].nodeType===8&&u.childNodes[o].nodeValue===a){o=o-1;break}n.units[a]={main:u.childNodes[o],parent:e.parent,scope:e.scope}}else n.units[a]={main:document.createTextNode(""),parent:e.parent,scope:e.scope},Rk(r,a,e.parent,n)};function Lv(){return{units:{},scopes:{}}}var Uk=t=>r=>e=>()=>{var n=r.id,a=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":e.units[n].main.setAttribute(r.key,a)},Wk=t=>r=>e=>()=>{var n=r.id,a=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=o=>a(o)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}},qk=t=>r=>()=>{var e=t.id;r.units[e].main.nodeValue=t.text},Hk=t=>r=>e=>n=>()=>{var a,u,o=e.id,i=e.html,m=e.verb,s=e.cache,c=e.parent,D=e.scope,b=e.pxScope;if(n.hydrating&&t&&e.parent.value0&&(a=document.body.querySelectorAll("[data-deku-ssr-"+o+"]").item(0)))n.units[o]={listeners:{},scope:D,parent:c,main:a};else{let h=Object.entries(s);for(var f=0;f<h.length;f++){let et=h[f][0];h[f][1]===!0?i=i.replace(m+et+m,'data-deku-attr-internal="'+et+'"'):i=i.replace(m+et+m,'<span style="display:contents;" data-deku-elt-internal="'+et+'"></span>')}u=document.createElement("div"),u.innerHTML=i.trim(),n.units[o]={listeners:{},scope:D,parent:c,main:u.firstChild}}n.scopes[D]||(n.scopes[D]=[]),n.scopes[D].push(o),u||(u=a),u.querySelectorAll("[data-deku-attr-internal]").forEach(function(h){var et=h.getAttribute("data-deku-attr-internal");let mt=et+b;n.units[mt]={listeners:{},main:h,scope:D},n.scopes[D].push(mt)}),u.querySelectorAll("[data-deku-elt-internal]").forEach(function(h){var et=h.getAttribute("data-deku-elt-internal");let mt=et+b;n.units[et+b]={listeners:{},main:h,scope:D},n.scopes[D].push(mt)}),a||Rk(r,o,c,n)},zk=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root}},Gk=t=>r=>()=>{var e=t.id,n=t.parent;r.units[e].containingScope=t.scope,r.units[n].main.prepend(r.units[e].main)},Vk=t=>r=>()=>{var e=t.id;r.units[e].noop||r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope)||r.units[e].main.remove()},Jk=t=>r=>()=>{delete r.units[t.id]},jk=t=>r=>()=>{var e=t.id,n=t.pos,a=r.units[e].main.parentNode;a.insertBefore(r.units[e].main,a.children.length<=n?a.children[a.children.length-1]:n<0?a.children[0]:a.children[n])};var Xk=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},zr=function(t){return t};var Bv=function(t){return function(r){return Math.pow(t,r)|0}};var Yp=isFinite;var o_=Math.floor;var tf=function(t){return function(r){return Math.pow(t,r)}},i_=function(t){return function(r){return t%r}},Zp=Math.round;var ts=Math.sin;var rf=3.141592653589793;var Sc=function(){return Xk(R.create)(z.value)}(),Kk=function(t){if(!Yp(t))return 0;if(t>=zr(Yn(yf)))return Yn(yf);if(t<=zr(Zn(yf)))return Zn(yf);if(ne)return ga(0)(Sc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Yk=function(t){return Kk(Zp(t))};var f_=function(t){return Kk(o_(t))};var Bu=Math.random;var c_=function(t){return function(r){return function(){var n=Bu(),a=(zr(r)-zr(t)+1)*n+zr(t);return f_(a)}}};var Zk=function(t){return t};var GO=1,rs=2147483647,VO=function(){return rs-1|0}(),Fc=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,o=ja(Qu)(a)(u),i=o<e;return i?o+n|0:o}}};return r(GO)(VO)(t)};var JO=0,jO=48271,tg=function(t){return function(r){return ra()(Sc(i_(zr(jO)*zr(r)+zr(t))(zr(rs))))}},rg=tg(JO);var e$=function(){function t(o){this.fn=o}var r={},e=function(o,i){this.head=o,this.tail=i};function n(o){return new e(o,r)}function a(o){return function(i){return new e(o,i)}}function u(o){for(var i=[],m=o;m!==r;)i.push(m.head),m=m.tail;return i}return function(o){return function(i){return function(m){var s=function(D,b){return o(i(a)(m(D)))(b)},c=function(D,b,f){if(b===0)return D;var h=f[b-1];return new t(function(){var et=c(s(h,D),b-1,f);return et})};return function(D){for(var b=i(n)(m(D[D.length-1])),f=c(b,D.length-1,D);f instanceof t;)f=f.fn();return i(u)(f)}}}}}();var og=function(t){return t};var ig=hn;var fg=Nt;var _g=og,__=function(t){return t};var p_=function(t){return _g(dA(t))};var Oc=function(t){if(Na(t)>0)return new R(_g(t));if(ne)return z.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var pg=function(t){return function(r){return t(__(r))}};var sg=pg(Na);var mg=function(){return pg(Qm())};var si=function(t){return t.state};function Bo(t){return new Error(t)}function Mf(t){return function(){throw t}}function as(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var oo=function(t){return t.throwError};var P$={throwError:Mf,Monad0:function(){return ri}};var Qv={catchError:St(as),MonadThrow0:function(){return P$}};var mi=function(t){return t.catchError};var m_=function(t){return function(r){return mi(t)(p(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Qt.create)(r))(function(){var e=_(t.MonadThrow0().Monad0().Applicative0());return function(n){return e(Xt.create(n))}}())}};var fe={liftEffect:Z(tt),Monad0:function(){return ri}},De=function(t){return t.liftEffect};var Yv=function(t){return{map:function(r){return function(e){return function(n){return p(t)(function(a){return new ut(r(a.value0),a.value1)})(e(n))}}}}};var Zv=function(t){return{Applicative0:function(){return rD(t)},Bind1:function(){return tD(t)}}},tD=function(t){return{bind:function(r){return function(e){return function(n){return W(t.Bind1())(r(n))(function(a){var u=e(a.value0);return u(a.value1)})}}},Apply0:function(){return cs(t)}}},cs=function(t){return{apply:Ju(Zv(t)),Functor0:function(){return Yv(t.Bind1().Apply0().Functor0())}}},rD=function(t){return{pure:function(r){return function(e){return _(t.Applicative0())(new ut(r,e))}},Apply0:function(){return cs(t)}}};var kg=function(t){return{state:function(r){var e=_(t.Applicative0());return function(n){return e(r(n))}},Monad0:function(){return Zv(t)}}};var Cg=function(t){return function(r){var e=t(r);return e.value0}};var q$=function(t){return t};var hg=function(){var t=function(r){return new ut(Zk(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=rg(r.newSeed),e}())};return si(kg(Xu))(t)}();var io=Yv(ho),Tg=p(io)(function(t){return zr(t)/zr(rs)})(hg);var wc=function(t){return Cg(q$(t))};var Pf=tD(Xu);var If=cs(Xu),Eg=function(t){return function(r){var e=zr(r),n=zr(t),a=function(i){return n+i_(i)(e-n+1)},u=p(io)(zr)(hg),o=It(If)(p(io)(Lr(Aa))(u))(p(io)(Pn(Aa)(2))(u));return p(io)(function(i){return f_(a(i))})(o)}},eD=function(t){return function(r){var e=t<=r;return e?Eg(t)(r):Eg(r)(t)}};var y_=rD(Xu);var nD=function(t){return W(Pf)(eD(0)(sg(t)-1|0))(function(r){return _(y_)(mg()(t)(r))})};var k_=function(t){return t.arbitrary};var Sg={arbitrary:Tg};var xg=function(){return{arbitrary:eD(-1e6)(1e6)}}();var Fg=function(t){return{ids:function(){var e=Pe(t)(),n=Gt(Ja)(wc(k_(xg))({newSeed:Fc(e),size:5}));return yr(F)(nc(Lr(xu)(1))(t))(),n},makeElement:Lk(!1),attributeParent:Nk,makeRoot:zk,makeText:Bk(!1)(Zt(void 0)),makePursx:Hk(!1)(Zt(void 0)),setProp:Uk(!1),setCb:Wk(!1),setText:qk,sendToPos:jk,deleteFromCache:Jk,giveNewParent:Gk,disconnectElement:Vk}};var j$=function(t){return t};var K=function(t){return{pursxToElement:function(r){return function(e){return function(n){return{cache:Ro,element:function(a){return function(u){return M(S(t))}}}}}}}},uD=function(t){return t.pursxToElement},an=function(){return function(t){return function(r){return function(e){return function(n){return{pursxToElement:function(a){return function(u){return function(o){var i=uD(t)(a)(y.value)(o);return{cache:pi(Io(r)(y.value))(!0)(i.cache),element:function(m){return function(s){return N(L(n.MonadST5().Monad0().Applicative0()))(p(C)(Ya(Un)(jp)(function(c){if(c.value instanceof a_)return s.setProp({id:Io(r)(y.value)+a,key:c.key,value:c.value.value0});if(c.value instanceof u_)return s.setCb({id:Io(r)(y.value)+a,key:c.key,value:c.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4475, column 38 - line 4485, column 24): "+[c.value.constructor.name])}))(li(e)()(y.value)(o)))(i.element(m)(s))}}}}}}}}}}}};var B=j$,sr=function(t){return function(r){return function(){return function(){return function(e){return function(n){return function(a){return function(u){return function(o){var i=function(m){return function(s){return qt(function(c){return W(e.MonadST5().Monad0().Bind1())(s.ids)(function(D){return W(e.MonadST5().Monad0().Bind1())(s.ids)(function(b){return te(Gr)(e.MonadST5().Monad0().Bind1())(m.raiseId(D))(function(){var f=uD(n)(b)(y.value)(o);return p(e.MonadST5().Monad0().Bind1().Apply0().Functor0())(X(e.MonadST5().Monad0().Bind1().Apply0())(c(s.deleteFromCache({id:D}))))(xt(N(L(e.MonadST5().Monad0().Applicative0()))(_(E(e.MonadST5()))(s.makePursx({id:D,parent:m.parent,cache:f.cache,pxScope:b,scope:m.scope,html:Io(t)(u),verb:Io(r)(a)})))(f.element(m)(s)))(c))})})})})}};return new P(i)}}}}}}}}},Ft=function(t){return function(){return function(){return function(r){return function(e){return sr(t)({reflectType:function(){return"~"}})()()(e)(r)(y.value)}}}}};var X$=function(t){return ao(t.MonadST5().Monad0().Applicative0())(t.MonadST5())({doLogic:function(r){return function(e){return function(n){return e.sendToPos({id:n,pos:r})}}},ids:function(){var r=ke();return function(e){return function(n){return n.ids}(r(e))}}(),disconnectElement:function(r){return function(e){return r.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:ar(Wp)})}},toElt:function(r){return r}})},U=function(){return function(t){return function(r){return function(e){return function(n){return{pursxToElement:function(a){return function(u){return function(o){var i=li(e)()(y.value)(o),m=uD(t)(a)(y.value)(o);return{cache:pi(Io(r)(y.value))(!1)(m.cache),element:function(s){return function(c){return N(L(n.MonadST5().Monad0().Applicative0()))(X$(n)({parent:new R(Io(r)(y.value)+a),scope:s.scope,raiseId:function(D){return _(n.MonadST5().Monad0().Applicative0())(void 0)},pos:s.pos})(c)(i))(m.element(s)(c))}}}}}}}}}}}};var yt=function(){return function(){return{defaults:St(rk()())}}},Q$=function(t){return t.defaults},At={convertRecordOptions:function(t){return function(r){return function(e){return Z(Qp)}}}},Og=function(t){return t.convertRecordOptions},_a=function(t){return t.convertOptionsWithDefaults},kt=function(){return function(t){return{convertOptions:function(r){return function(e){return Ok(Og(t)(r)(y.value)(e))}}}}},K$=function(t){return t.convertOptions},gt=function(t){return function(r){return{convertOptionsWithDefaults:function(e){return function(n){var a=Q$(r)(n),u=K$(t)(e);return function(o){return a(u(o))}}}}}},Y$=function(t){return t.convertOption},J=function(t){return function(r){return function(){return function(){return function(){return function(e){return{convertRecordOptions:function(n){return function(a){return function(u){return $u(Xp)(Mv()()(e)(y.value)(Y$(r)(n)(y.value)(li(e)()(y.value)(u))))(Og(t)(n)(y.value)(u))}}}}}}}}}};var oD=function(){var t=Tp(rv);return function(r){return Sp(t(r))}}();var Kz=typeof Array.from=="function",Yz=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",Zz=typeof String.prototype.fromCodePoint=="function",tG=typeof String.prototype.codePointAt=="function";var vi={proof:function(t){return t},Coercible0:function(){}},fD=function(t){return t.proof};var Au=void 0;var ds=function(t){return t.toInt},Pg=function(t){return function(r){return ds(t)(Au)}};var ru={toInt:function(t){return 8}},Ig={Nat0:function(){return ru}},Uo={toInt:function(t){return 7}},Rg={Nat0:function(){return Uo}},Wo={toInt:function(t){return 6}},Ng={Nat0:function(){return Wo}},Sa={toInt:function(t){return 5}},bs={Nat0:function(){return Sa}},qn={toInt:function(t){return 4}},ua={Nat0:function(){return qn}},Hn={toInt:function(t){return 3}},ku={Nat0:function(){return Hn}},zn={toInt:function(t){return 2}},gu={Nat0:function(){return zn}},Gn={toInt:function(t){return 1}},Cu={Nat0:function(){return Gn}},Fe={toInt:function(t){return 0}};var Or=function(t){return function(){return function(r){return function(){return function(e){return{Nat0:r.Nat1,Pos1:function(){return t}}}}}}};var fo={Nat0:function(){return Uo},Nat1:function(){return ru}};var co={Nat0:function(){return Wo},Nat1:function(){return ru}};var lo={Nat0:function(){return Sa},Nat1:function(){return ru}};var _o={Nat0:function(){return qn},Nat1:function(){return ru}};var pa={Nat0:function(){return qn},Nat1:function(){return Sa}};var po={Nat0:function(){return Hn},Nat1:function(){return ru}};var sa={Nat0:function(){return Hn},Nat1:function(){return Sa}};var so={Nat0:function(){return zn},Nat1:function(){return ru}};var ma={Nat0:function(){return zn},Nat1:function(){return Sa}};var mo={Nat0:function(){return Gn},Nat1:function(){return ru}};var va={Nat0:function(){return Gn},Nat1:function(){return Sa}};var vo={Nat0:function(){return Fe},Nat1:function(){return ru}};var Da={Nat0:function(){return Fe},Nat1:function(){return Sa}};var Lg={Nat0:function(){return Fe},Nat1:function(){return ru}};var cD={Nat0:function(){return Fe},Nat1:function(){return Uo}};var lD={Nat0:function(){return Fe},Nat1:function(){return Wo}};var C_={Nat0:function(){return Fe},Nat1:function(){return Sa}};var qa={Nat0:function(){return Fe},Nat1:function(){return qn}};var mn={Nat0:function(){return Fe},Nat1:function(){return Hn}};var vn={Nat0:function(){return Fe},Nat1:function(){return zn}};var Dn={Nat0:function(){return Fe},Nat1:function(){return Gn}},Eu={Nat0:function(){return Fe},Nat1:function(){return Fe}};var Bg=Fo;var ys=function(t){return t};var E_=function(t){return function(){return function(r){return function(e){return r[ds(t)(e)]}}}};var As=function(t){return function(r){var e=Pg(t)(y.value),n=function(){return e===0?[]:ln(0)(e-1|0)}();return p(Fr)(r)(n)}};var Uu=[];var $r=function(t){return function(r){return function(e){return Hi(r)(e)}}};var dn={first:function(t){return function(r){return new ut(t(r.value0),r.value1)}},second:p(ei),Profunctor0:function(){return Un}},Vn=function(t){return t.second},ks=function(t){return t.first};var FM=function(t){return function(r){return function(e){return function(n){return Po(e)(t)(r)(n)}}}};var Hg=function(){return function(){return function(t){return FM(Nn())(Nn())(t)}}};var zg=function(){return function(){return function(t){return Hg()()(t)}}};var MM=function(t){return function(r){return function(e){return Po(r.Profunctor0())(t)(function(n){return n.value1(n.value0)})(ks(r)(e))}}},Gg=function(t){return function(r){return function(e){return MM(function(n){return new ut(t(n),function(a){return r(n)(a)})})(e)}}};var Vg=function(t){return function(){return function(){return function(r){return function(e){return Gg(li(t)()(r))(St(tk(t)()()(r)))(e)}}}}};var Jg=function(t){return t};var LM=JSON.parse;var BM=JSON.stringify;var gs=function(t){return t};var Cs=function(t){return t};var Es=function(t){return function(r){return t(r)}},h_=function(t){return{map:function(r){return Es(p(t)(p(kf)(r)))}}};var sD=function(t){return{Applicative0:function(){return T_(t)},Bind1:function(){return mD(t)}}},mD=function(t){return{bind:function(r){return function(e){return W(t.Bind1())(r)(Ia(function(){var n=_(t.Applicative0());return function(a){return n(Xt.create(a))}}())(function(n){var a=e(n);return a}))}},Apply0:function(){return Qg(t)}}},Qg=function(t){return{apply:Ju(sD(t)),Functor0:function(){return h_(t.Bind1().Apply0().Functor0())}}},T_=function(t){return{pure:function(){var r=_(t.Applicative0());return function(e){return gs(r(Qt.create(e)))}}(),Apply0:function(){return Qg(t)}}};var Kg=function(t){return{throwError:function(){var r=_(t.Applicative0());return function(e){return gs(r(Xt.create(e)))}}(),Monad0:function(){return sD(t)}}};var vD=function(t){return function(r){return{alt:function(e){return function(n){return W(r.Bind1())(e)(function(a){if(a instanceof Qt)return _(r.Applicative0())(new Qt(a.value0));if(a instanceof Xt)return W(r.Bind1())(n)(function(u){if(u instanceof Qt)return _(r.Applicative0())(new Qt(u.value0));if(u instanceof Xt)return _(r.Applicative0())(new Xt(dt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return h_(r.Bind1().Apply0().Functor0())}}}};var DD=function(){var t=ke();return function(r){return t(Cs(r))}}();function Zg(t,r,e){return t==null?r:e(t)}var Ke=function(t){return Zg(t,z.value,R.create)};function S_(t){return Object.prototype.toString.call(t).slice(8,-1)}var uC=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var kD=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var oC=_t;var gD=function(t){var r=oo(Kg(t));return function(e){return r(oD(e))}};var CD=function(t){return function(r){return function(e){if(S_(e)===r)return _(T_(t))(oC(e));if(ne)return gD(t)(new kD(r,S_(e)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[r.constructor.name,e.constructor.name])}}};var ED=function(t){return CD(t)("String")};var Ss=function(){function t(){}return t.value=new t,t}(),xs=function(){function t(){}return t.value=new t,t}(),cC=function(){function t(){}return t.value=new t,t}(),lC=function(){function t(){}return t.value=new t,t}(),TD=function(){function t(){}return t.value=new t,t}(),_C=function(){function t(){}return t.value=new t,t}(),pC=function(){function t(){}return t.value=new t,t}();var sC=function(t){return t},mC=function(t){return t};var vC=function(t){return t};var DC=function(t){return t};var dC=function(t){return t};var bC=function(t){return t},yC=function(t){return t},AC=function(t){return t},kC=function(t){return t},gC=function(t){return t};var SD=function(){function t(){}return t.value=new t,t}(),CC=function(){function t(){}return t.value=new t,t}(),EC=function(){function t(){}return t.value=new t,t}(),xD=function(){function t(){}return t.value=new t,t}(),hC=function(){function t(){}return t.value=new t,t}();var Fs=function(t){return t};var Nc=function(t){return t};var kw=function(t){return t},x_=function(t){return t};var Lf={toAudioOnOff:Z(tt)};var Bf=function(t){return t.toAudioParameter},TC=function(t){return t.toAudioOnOff},SC=function(){return kc.create}(),xC=function(){return gc.value}();var Os=function(){return Jg(function(){var t=zg()()(Un),r=Vg({reflectSymbol:function(){return"o"}})()()(y.value)(dn);return function(e){return t(r(e))}}())},FC=_t;var gw=function(){var t=ie()({reflectSymbol:function(){return"unit"}})(y.value);return function(r){return x_(t(r))}}();var Cw=function(t){return function(r){return{toAudioParameter:function(e){return gw(e)}}}},OC=function(t){return function(r){return{toAudioParameter:function(){var e=Bf(Cw(t)(r));return function(n){return e(kw(function(a){return{u:a}}(n)))}}()}}},$C=function(){return ie()({reflectSymbol:function(){return"2x"}})(y.value)(void 0)}(),MC=function(){var t=ie()({reflectSymbol:function(){return"sudden"}})(y.value);return function(r){return x_(t(r))}}();var wC={toAudioParameter:MC},$s={toAudioParameter:function(t){return MC({n:t})}},FD=function(){return ie()({reflectSymbol:function(){return"step"}})(y.value)(void 0)}();var OD=function(){return ie()({reflectSymbol:function(){return"on"}})(y.value)(void 0)}(),F_={x:OD,o:0},pt=function(){return _(E(d))(rn()(ie()({reflectSymbol:function(){return"onOff"}})(y.value)(F_)))};var PC=function(){return ie()({reflectSymbol:function(){return"off"}})(y.value)(void 0)}();var Ew=function(){var t=ie()({reflectSymbol:function(){return"numeric"}})(y.value);return function(r){return x_(t(r))}}();var Ne={toAudioParameter:Ew};var qo=function(){return ie()({reflectSymbol:function(){return"linear"}})(y.value)(void 0)}();var IC=function(){return ie()({reflectSymbol:function(){return"exponential"}})(y.value)(void 0)}(),hw=function(){var t=ie()({reflectSymbol:function(){return"envelope"}})(y.value);return function(r){return x_(t(r))}}();var $n={toAudioParameter:hw},Tw=function(){var t=ie()({reflectSymbol:function(){return"cancel"}})(y.value);return function(r){return x_(t(r))}}();var RC={toAudioParameter:Tw};var Sw=function(){function t(){}return t.value=new t,t}(),xw=function(){function t(){}return t.value=new t,t}(),Fw=function(){function t(){}return t.value=new t,t}(),Ow=function(){function t(){}return t.value=new t,t}(),$w=function(){function t(){}return t.value=new t,t}(),Mw=function(){function t(){}return t.value=new t,t}(),ww=function(){function t(){}return t.value=new t,t}(),Pw=function(){function t(){}return t.value=new t,t}(),Iw=function(){function t(){}return t.value=new t,t}(),Rw=function(){function t(){}return t.value=new t,t}(),Nw=function(){function t(){}return t.value=new t,t}(),Lw=function(){function t(){}return t.value=new t,t}(),Bw=function(){function t(){}return t.value=new t,t}(),Uw=function(){function t(){}return t.value=new t,t}(),Di=function(t){return{toPeriodicOscSpec:function(r){return ie()({reflectSymbol:function(){return"realImg"}})(y.value)({real:ys(r.value0),img:ys(r.value1)})}}};var Ms={toInitializeTriangleOsc:function(t){return gC(function(r){return{frequency:r}}(t))}};var NC={toInitializeStereoPanner:function(t){return kC(function(r){return{pan:r}}(t))}};var Lc={toInitializeSquareOsc:function(t){return AC(function(r){return{frequency:r}}(t))}};var af={toInitializeSinOsc:function(t){return yC(function(r){return{frequency:r}}(t))}};var LC={toInitializeSawtoothOsc:function(t){return bC(function(r){return{frequency:r}}(t))}};var $D={toInitializeRecorder:function(t){return sC(function(r){return{cb:r}}(t))}};var O_={toInitializeMicrophone:function(t){return mC(function(r){return{microphone:r}}(t))}};var BC=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:fD(vi)(Nn()(e.value0)),feedback:fD(vi)(Nn()(e.value1))}}}}}}};var it={toInitializeGain:function(t){return dC(function(r){return{gain:r}}(t))}};var UC={toInitializeConvolver:function(t){return vC(function(r){return{buffer:r}}(t))}},ws={toInitializeConstant:function(t){return DC(function(r){return{offset:r}}(t))}};var Ww={convertOption:function(t){return function(r){return Z(tt)}}},$_={convertOption:function(t){return function(r){return Z(tt)}}},WC={convertOption:function(t){return function(r){return Z(tt)}}},qC={convertOption:function(t){return function(r){return R.create}}},HC={convertOption:function(t){return function(r){return Z(tt)}}},di={convertOption:function(t){return function(r){return Z(tt)}}},Bc={convertOption:function(t){return function(r){return Z(tt)}}},Uc={convertOption:function(t){return function(r){return Z(tt)}}},Wc={convertOption:function(t){return function(r){return Z(tt)}}},qc={convertOption:function(t){return function(r){return Z(tt)}}},Hc={convertOption:function(t){return function(r){return Z(tt)}}},zC={convertOption:function(t){return function(r){return Z(tt)}}},GC={convertOption:function(t){return function(r){return Z(tt)}}},VC={convertOption:function(t){return function(r){return Z(tt)}}},MD={convertOption:function(t){return function(r){return Z(tt)}}},Uf={convertOption:function(t){return function(r){return Z(tt)}}},M_={convertOption:function(t){return function(r){return Z(tt)}}},w_={convertOption:function(t){return function(r){return Z(tt)}}};var zc={convertOption:function(t){return function(r){return Z(tt)}}},JC={convertOption:function(t){return function(r){return Z(tt)}}},jC={convertOption:function(t){return function(r){return Z(tt)}}},XC={convertOption:function(t){return function(r){return Z(tt)}}},wD={convertOption:function(t){return function(r){return Z(tt)}}};var QC={convertOption:function(t){return function(r){return Z(tt)}}},PD={convertOption:function(t){return function(r){return Z(tt)}}},yn={convertOption:function(t){return function(r){return Z(tt)}}},un={convertOption:function(t){return function(r){return Z(tt)}}},ID={convertOption:function(t){return function(r){return Z(tt)}}},Ps={convertOption:function(t){return function(r){return Z(tt)}}},qw=function(t){return t.toPeriodicOscSpec},bi=function(t){return{convertOption:function(r){return function(e){return qw(t)}}}},RD=function(t){return t.toInitializeWaveShaper},KC=function(t){return t.toInitializeTriangleOsc},YC=function(t){return t.toInitializeStereoPanner},ZC=function(t){return t.toInitializeSquareOsc},tE=function(t){return t.toInitializeSinOsc},rE=function(t){return t.toInitializeSawtoothOsc},eE=function(t){return t.toInitializeRecorder},ND=function(t){return t.toInitializePlayBuf},nE=function(t){return t.toInitializePeriodicOsc},aE=function(t){return t.toInitializePeaking},uE=function(t){return t.toInitializeNotch},oE=function(t){return t.toInitializeMicrophone},iE=function(t){return t.toInitializeLowshelf},LD=function(t){return t.toInitializeLowpass},BD=function(t){return t.toInitializeLoopBuf},fE=function(t){return t.toInitializeIIRFilter},cE=function(t){return t.toInitializeHighshelf},UD=function(t){return t.toInitializeHighpass},lE=function(t){return t.toInitializeGain},_E=function(t){return t.toInitializeDynamicsCompressor},WD=function(t){return t.toInitializeDelay},pE=function(t){return t.toInitializeConvolver},sE=function(t){return t.toInitializeConstant},qD=function(t){return t.toInitializeBandpass},HD=function(t){return t.toInitializeAllpass};var Hw={oversample:$C},zw=function(t){return{toInitializeWaveShaper:function(r){return _a(t)(Sw.value)(Hw)(r)}}},mE={toInitializeWaveShaper:function(){var t=RD(zw(gt(kt()(J(At)(Ww)()()()({reflectSymbol:function(){return"curve"}})))(yt()())));return function(r){return t(function(e){return{curve:e}}(r))}}()},Gw=function(){return{bufferOffset:0,playbackRate:1,duration:z.value}}(),P_=function(t){return{toInitializePlayBuf:function(r){return _a(t)(xw.value)(Gw)(r)}}},Ha={toInitializePlayBuf:function(){var t=ND(P_(gt(kt()(J(At)($_)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},Vw={},yi=function(t){return{toInitializePeriodicOsc:function(r){return _a(t)(Fw.value)(Vw)(r)}}},Jw={q:1,gain:0},Gc=function(t){return{toInitializePeaking:function(r){return _a(t)(Ow.value)(Jw)(r)}}};var jw={q:1},Vc=function(t){return{toInitializeNotch:function(r){return _a(t)($w.value)(jw)(r)}}};var Xw={gain:0},vE=function(t){return{toInitializeLowshelf:function(r){return _a(t)(Mw.value)(Xw)(r)}}};var Qw={q:1},zD=function(t){return{toInitializeLowpass:function(r){return _a(t)(ww.value)(Qw)(r)}}},Is={toInitializeLowpass:function(){var t=LD(zD(gt(kt()(J(At)(MD)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},Kw=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:z.value}}(),Wf=function(t){return{toInitializeLoopBuf:function(r){return _a(t)(Pw.value)(Kw)(r)}}},gr={toInitializeLoopBuf:function(){var t=BD(Wf(gt(kt()(J(At)(Uf)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},Yw={gain:0},DE=function(t){return{toInitializeHighshelf:function(r){return _a(t)(Iw.value)(Yw)(r)}}};var Zw={q:1},GD=function(t){return{toInitializeHighpass:function(r){return _a(t)(Rw.value)(Zw)(r)}}},eu={toInitializeHighpass:function(){var t=UD(GD(gt(kt()(J(At)(wD)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},tP=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),dE=function(t){return{toInitializeDynamicsCompressor:function(r){return _a(t)(Nw.value)(tP)(r)}}},rP={maxDelayTime:1},VD=function(t){return{toInitializeDelay:function(r){return _a(t)(Lw.value)(rP)(r)}}},Ye={toInitializeDelay:function(){var t=WD(VD(gt(kt()(J(At)(PD)()()()({reflectSymbol:function(){return"delayTime"}})))(yt()())));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},eP={q:1},on=function(t){return{toInitializeBandpass:function(r){return _a(t)(Bw.value)(eP)(r)}}},JD={toInitializeBandpass:function(){var t=qD(on(gt(kt()(J(At)(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},nP={q:1},Rs=function(t){return{toInitializeAllpass:function(r){return _a(t)(Uw.value)(nP)(r)}}},jD={toInitializeAllpass:function(){var t=HD(Rs(gt(kt()(J(At)(Ps)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var Ho=function(){function t(){this.head=null,this.last=null,this.size=0}function r(c,D){this.queue=c,this.value=D,this.next=null,this.prev=null}function e(c){this.draining=!1,this.error=null,this.value=c,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(c){try{c()}catch(D){setTimeout(function(){throw D},0)}}function u(c,D){var b=new r(c,D);switch(c.size){case 0:c.head=b;break;case 1:b.prev=c.head,c.head.next=b,c.last=b;break;default:b.prev=c.last,c.last.next=b,c.last=b}return c.size++,b}function o(c){var D;switch(c.size){case 0:return null;case 1:D=c.head,c.head=null;break;case 2:D=c.last,c.head.next=null,c.last=null;break;default:D=c.last,c.last=D.prev,c.last.next=null}return D.prev=null,D.queue=null,c.size--,D.value}function i(c){var D;switch(c.size){case 0:return null;case 1:D=c.head,c.head=null;break;case 2:D=c.head,c.last.prev=null,c.head=c.last,c.last=null;break;default:D=c.head,c.head=D.next,c.head.prev=null}return D.next=null,D.queue=null,c.size--,D.value}function m(c){if(c.queue!==null){if(c.queue.last===c){o(c.queue);return}if(c.queue.head===c){i(c.queue);return}c.prev&&(c.prev.next=c.next),c.next&&(c.next.prev=c.prev),c.queue.size--,c.queue=null,c.value=null,c.next=null,c.prev=null}}function s(c,D){if(!D.draining){var b=D.puts,f=D.takes,h=D.reads,et,mt,ur,jt,jr;for(D.draining=!0;;){if(et=null,mt=null,ur=null,jt=D.value,jr=h.size,D.error!==null){for(jt=c.left(D.error);et=i(b);)a(et.cb(jt));for(;mt=i(h);)a(mt(jt));for(;ur=i(f);)a(ur(jt));break}if(jt===n&&(et=i(b))&&(D.value=jt=et.value),jt!==n){for(ur=i(f);jr--&&(mt=i(h));)a(mt(c.right(jt)));ur!==null&&(D.value=n,a(ur(c.right(jt))))}if(et!==null&&a(et.cb(c.right(void 0))),D.value===n&&b.size===0||D.value!==n&&f.size===0)break}D.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=o,e.takeHead=i,e.deleteCell=m,e.drainVar=s,e}();function I_(){return new Ho(Ho.EMPTY)}function bE(t,r,e){return function(){var n=Ho.putLast(r.takes,e);return Ho.drainVar(t,r),function(){Ho.deleteCell(n)}}}function yE(t,r,e){return function(){return e.value===Ho.EMPTY&&e.error===null?(e.value=r,Ho.drainVar(t,e),!0):!1}}function AE(t,r){return function(){var e=r.value;return e===Ho.EMPTY?t.nothing:(r.value=Ho.EMPTY,Ho.drainVar(t,r),t.just(e))}}var iP=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),fP=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),cP=function(){function t(){}return t.value=new t,t}();var XD=function(){return{left:Xt.create,right:Qt.create,nothing:z.value,just:R.create,killed:iP.create,filled:fP.create,empty:cP.value}}();var kE=function(t){return function(r){return bE(XD,t,r)}},Ns=function(t){return function(r){return yE(XD,t,r)}};var gE=function(t){return AE(XD,t)};var lP=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var _P=function(){function t(){}return t.value=new t,t}();var Ls={convertOption:function(t){return function(r){return Z(tt)}}},Bs={convertOption:function(t){return function(r){return Z(tt)}}};var pP=function(t){return t.toInitializeAnalyser},nu=kk(d)({doLogic:bf,ids:function(){var t=ke();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:Nn(),connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var sP=function(){return{cb:function(t){return _(l)(_(l)(void 0))},fftSize:TD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:xD.value,channelInterpretation:SD.value}}(),Us=function(t){return{toInitializeAnalyser:function(r){return _a(t)(_P.value)(sP)(r)}}};var mP=function(t){return function(r){var e=oE(t)(r),n=function(a){return function(u){return qt(function(o){return function(){var m=u.ids();return a.raiseId(m)(),p(F)(function(s){return X(at)(o(u.deleteFromCache({id:m})))(s)})(St(xt)(o)(_(E(d))(u.makeMicrophone({id:m,parent:a.parent,scope:a.scope,microphone:e.microphone}))))()}})}};return new P(n)}},R_=function(t){return mP(t)};var Jn=ao(l)(d)({doLogic:bf,ids:function(){var t=ke();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),vP=function(t){return function(r){return function(e){return function(n){var a=pP(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeAnalyser({id:c,parent:o.parent,scope:o.scope,cb:a.cb,fftSize:Bv(2)(function(){if(a.fftSize instanceof Ss)return 7;if(a.fftSize instanceof xs)return 8;if(a.fftSize instanceof cC)return 9;if(a.fftSize instanceof lC)return 10;if(a.fftSize instanceof TD)return 11;if(a.fftSize instanceof _C)return 12;if(a.fftSize instanceof pC)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 189, column 21 - line 196, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof hC)return"explicit";if(a.channelCountMode instanceof xD)return"max";if(a.channelCountMode instanceof EC)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 202, column 35 - line 205, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof SD)return"speakers";if(a.channelInterpretation instanceof CC)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 206, column 40 - line 208, column 41): "+[a.channelInterpretation.constructor.name])}()})))(N(L(l))(p(C)(function(D){return Ue()()()({cb:function(b){return i.setAnalyserNodeCb({id:c,cb:b})}})(D)})(e))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},Ws=function(t){return function(r){return vP(t)(r)(M(S(l)))}},EE=function(t){return function(r){return function(e){var n=pE(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(Jn({parent:new R(s),scope:u.scope,raiseId:$t(Xe(Ae(we)))})(o)(q(e)))))()}})}};return new P(a)}}},DP=function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){var u=fE(t)(n)(r)(e),o=function(i){return function(m){return qt(function(s){return function(){var D=m.ids();return i.raiseId(D)(),p(F)(function(b){return X(at)(s(m.deleteFromCache({id:D})))(b)})(St(xt)(s)(N(L(l))(_(E(d))(m.makeIIRFilter({id:D,parent:i.parent,scope:i.scope,feedforward:xf()(u.feedforward),feedback:xf()(u.feedback)})))(Jn({parent:new R(D),scope:i.scope,raiseId:$t(Xe(Ae(we)))})(m)(q(a)))))()}})}};return new P(o)}}}}}}},hE=function(){return function(){return function(t){return DP()()(t)(y.value)(y.value)}}},QD=function(t){return function(r){return function(e){var n=eE(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(Jn({parent:new R(s),scope:u.scope,raiseId:$t(Xe(Ae(we)))})(o)(e))))()}})}};return new P(a)}}},dP=function(t){return function(r){return qt(function(e){return function(){var a=r.ids();return e(r.makeSpeaker({id:a}))(),xt(Jn({parent:new R(a),scope:new Zi("toplevel"),raiseId:$t(Xe(Ae(we)))})(r)(q(t)))(e)()}})}},qf=dP,Mt=function(t){return function(r){return function(e){return We(t)(r)(M(S(l)))(e)}}},We=function(t){return function(r){return function(e){return function(n){var a=lE(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeGain({id:c,parent:o.parent,scope:o.scope,gain:a.gain})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({gain:TE(591)(o.scope)(i)(function(b){return i.setGain(function(f){return{id:c,gain:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},TE=lP("tmpResolveAU","Ocarina.Control",function(){var t=function(){var o=ie()({reflectSymbol:function(){return"unit"}})(y.value);return function(i){return Nc(o(i))}}(),r=function(){var o=ie()({reflectSymbol:function(){return"sudden"}})(y.value);return function(i){return Nc(o(i))}}(),e=function(){var o=ie()({reflectSymbol:function(){return"numeric"}})(y.value);return function(i){return Nc(o(i))}}(),n=function(){var o=ie()({reflectSymbol:function(){return"envelope"}})(y.value);return function(i){return Nc(o(i))}}(),a=function(){var o=ie()({reflectSymbol:function(){return"cancel"}})(y.value);return function(i){return Nc(o(i))}}(),u=function(o){return function(i){return function(m){return function(s){return Ue()()()({numeric:function(){var c=_(E(d));return function(D){return c(m(e(D)))}}(),envelope:function(){var c=_(E(d));return function(D){return c(m(n(D)))}}(),cancel:function(){var c=_(E(d));return function(D){return c(m(a(D)))}}(),sudden:function(){var c=_(E(d));return function(D){return c(m(r(D)))}}(),unit:function(c){var D=Mt(it)(1)([c.u]);return qt(function(b){return function(){var h=I_();return xt(N(L(l))(Jn({parent:z.value,scope:o,raiseId:function(et){return yr(F)(Ns(et)(h))}})(i)(D))(qt(function(et){return function(){return yr(F)(kE(h)(function(ur){if(ur instanceof Xt)return Mf(ur.value0);if(ur instanceof Qt)return et(m(t({i:ur.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1674, column 39 - line 1677, column 66): "+[ur.constructor.name])}))(),_(l)(void 0)}})))(b)()}})}})(s)}}}};return u}),ce=TE(1653),bP=function(t){return function(r){return function(e){var n=BD(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({buffer:function(D){return _(E(d))(o.setBuffer({id:s,buffer:D}))},playbackRate:ce(u.scope)(o)(function(D){return o.setPlaybackRate(function(b){return{id:s,playbackRate:b}}(D))}),loopStart:function(D){return _(E(d))(o.setLoopStart({id:s,loopStart:D}))},loopEnd:function(D){return _(E(d))(o.setLoopEnd({id:s,loopEnd:D}))},onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))}})(c)})(e)))))()}})}};return new P(a)}}},lr=function(t){return bP(t)};var yP=function(t){return function(r){return function(e){var n=nE(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({frequency:ce(u.scope)(o)(function(D){return o.setFrequency(function(b){return{id:s,frequency:b}}(D))}),onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))},spec:function(D){return _(E(d))(o.setPeriodicOsc({id:s,spec:D}))}})(c)})(e)))))()}})}};return new P(a)}}},Ai=function(t){return yP(t)};var AP=function(t){return function(r){return function(e){var n=ND(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({buffer:function(D){return _(E(d))(o.setBuffer({id:s,buffer:D}))},playbackRate:ce(u.scope)(o)(function(D){return o.setPlaybackRate(function(b){return{id:s,playbackRate:b}}(D))}),bufferOffset:function(D){return _(E(d))(o.setBufferOffset({id:s,bufferOffset:D}))},onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))},duration:function(D){return _(E(d))(o.setDuration({id:s,duration:D}))}})(c)})(e)))))()}})}};return new P(a)}}},jn=function(t){return AP(t)};var kP=function(t){return function(r){return function(e){var n=rE(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({frequency:ce(u.scope)(o)(function(D){return o.setFrequency(function(b){return{id:s,frequency:b}}(D))}),onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))}})(c)})(e)))))()}})}};return new P(a)}}},SE=function(t){return kP(t)};var gP=function(t){return function(r){return function(e){var n=tE(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({frequency:ce(u.scope)(o)(function(D){return o.setFrequency(function(b){return{id:s,frequency:b}}(D))}),onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))}})(c)})(e)))))()}})}};return new P(a)}}},Hf=function(t){return gP(t)},xE=function(t){return function(r){return Hf(t)(r)(M(S(l)))}},CP=function(t){return function(r){return function(e){var n=ZC(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({frequency:ce(u.scope)(o)(function(D){return o.setFrequency(function(b){return{id:s,frequency:b}}(D))}),onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))}})(c)})(e)))))()}})}};return new P(a)}}},N_=function(t){return CP(t)},FE=function(t){return function(r){return N_(t)(r)(M(S(l)))}},EP=function(t){return function(r){return function(e){var n=KC(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({frequency:ce(u.scope)(o)(function(D){return o.setFrequency(function(b){return{id:s,frequency:b}}(D))}),onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))}})(c)})(e)))))()}})}};return new P(a)}}},qs=function(t){return EP(t)};var hP=function(t){return function(r){return function(e){return function(n){var a=HD(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeAllpass({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,q:a.q})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),q:ce(o.scope)(i)(function(b){return i.setQ(function(f){return{id:c,q:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},L_=function(t){return function(r){return function(e){return hP(t)(r)(M(S(l)))(e)}}},KD=function(t){return function(r){return function(e){return function(n){var a=qD(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeBandpass({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,q:a.q})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),q:ce(o.scope)(i)(function(b){return i.setQ(function(f){return{id:c,q:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},An=function(t){return function(r){return function(e){return KD(t)(r)(M(S(l)))(e)}}},B_=function(t){return function(r){return function(e){return function(n){var a=WD(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeDelay({id:c,parent:o.parent,scope:o.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({delayTime:ce(o.scope)(i)(function(b){return i.setDelay(function(f){return{id:c,delayTime:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},Do=function(t){return function(r){return function(e){return B_(t)(r)(M(S(l)))(e)}}},TP=function(t){return function(r){return function(e){return function(n){var a=_E(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeDynamicsCompressor({id:c,parent:o.parent,scope:o.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({threshold:ce(o.scope)(i)(function(b){return i.setThreshold(function(f){return{id:c,threshold:f}}(b))}),ratio:ce(o.scope)(i)(function(b){return i.setRatio(function(f){return{id:c,ratio:f}}(b))}),knee:ce(o.scope)(i)(function(b){return i.setKnee(function(f){return{id:c,knee:f}}(b))}),attack:ce(o.scope)(i)(function(b){return i.setAttack(function(f){return{id:c,attack:f}}(b))}),release:ce(o.scope)(i)(function(b){return i.setRelease(function(f){return{id:c,release:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},OE=function(t){return function(r){return TP(t)(r)(M(S(l)))}},SP=function(){return function(t){return function(r){return Ov()(d)({doLogic:bf,ids:function(){var e=ke();return function(n){return function(a){return a.ids}(e(n))}}(),disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:Nn(),fromEltO2:Nn(),toElt:Nn(),wrapElt:function(e){return Mt(it)(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var e=ke();return function(n){return function(a){return a.deleteFromCache}(e(n))}}()})(t)(Ya(Un)(p(r_)(function(e){return e(void 0)}))(Nn()(r)))}}},xa=function(t){return function(r){return SP()(ck(t))(Ya(Un)(lk()()()()()({reflectType:function(){return 0}})(y.value))(r))}};var YD=function(t){return function(r){return function(e){return function(n){var a=UD(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeHighpass({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,q:a.q})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),q:ce(o.scope)(i)(function(b){return i.setQ(function(f){return{id:c,q:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},Jc=function(t){return function(r){return function(e){return YD(t)(r)(M(S(l)))(e)}}},xP=function(t){return function(r){return function(e){return function(n){var a=cE(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeHighshelf({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,gain:a.gain})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),gain:ce(o.scope)(i)(function(b){return i.setGain(function(f){return{id:c,gain:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},$E=function(t){return function(r){return function(e){return xP(t)(r)(M(S(l)))(e)}}},ME=function(t){return function(r){return function(e){return function(n){var a=LD(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeLowpass({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,q:a.q})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),q:ce(o.scope)(i)(function(b){return i.setQ(function(f){return{id:c,q:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},jc=function(t){return function(r){return function(e){return ME(t)(r)(M(S(l)))(e)}}},FP=function(t){return function(r){return function(e){return function(n){var a=iE(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeLowshelf({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,gain:a.gain})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),gain:ce(o.scope)(i)(function(b){return i.setGain(function(f){return{id:c,gain:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},wE=function(t){return function(r){return function(e){return FP(t)(r)(M(S(l)))(e)}}},OP=function(t){return function(r){return function(e){return function(n){var a=uE(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeNotch({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,q:a.q})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),q:ce(o.scope)(i)(function(b){return i.setQ(function(f){return{id:c,q:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},Xc=function(t){return function(r){return function(e){return OP(t)(r)(M(S(l)))(e)}}},$P=function(t){return function(r){return function(e){return function(n){var a=YC(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makeStereoPanner({id:c,parent:o.parent,scope:o.scope,pan:a.pan})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({pan:ce(o.scope)(i)(function(b){return i.setPan(function(f){return{id:c,pan:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},PE=function(t){return function(r){return $P(t)(r)(M(S(l)))}},MP=function(t){return function(r){return function(e){return function(n){var a=aE(t)(r),u=function(o){return function(i){return qt(function(m){return function(){var c=i.ids();return o.raiseId(c)(),p(F)(function(D){return X(at)(m(i.deleteFromCache({id:c})))(D)})(St(xt)(m)(N(L(l))(_(E(d))(i.makePeaking({id:c,parent:o.parent,scope:o.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(N(L(l))(xe(Lt(d))(p(C)(function(D){return Ue()()()({frequency:ce(o.scope)(i)(function(b){return i.setFrequency(function(f){return{id:c,frequency:f}}(b))}),q:ce(o.scope)(i)(function(b){return i.setQ(function(f){return{id:c,q:f}}(b))}),gain:ce(o.scope)(i)(function(b){return i.setGain(function(f){return{id:c,gain:f}}(b))})})(D)})(e)))(Jn({parent:new R(c),scope:o.scope,raiseId:$t(Xe(Ae(we)))})(i)(q(n))))))()}})}};return new P(u)}}}},Qc=function(t){return function(r){return function(e){return MP(t)(r)(M(S(l)))(e)}}},IE=function(t){return function(r){return function(e){var n=RD(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(Jn({parent:new R(s),scope:u.scope,raiseId:$t(Xe(Ae(we)))})(o)(q(e)))))()}})}};return new P(a)}}},wP=function(t){return function(r){return function(e){var n=sE(t)(r),a=function(u){return function(o){return qt(function(i){return function(){var s=o.ids();return u.raiseId(s)(),p(F)(function(c){return X(at)(i(o.deleteFromCache({id:s})))(c)})(St(xt)(i)(N(L(l))(_(E(d))(o.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))(xe(Lt(d))(p(C)(function(c){return Ue()()()({offset:ce(u.scope)(o)(function(D){return o.setOffset(function(b){return{id:s,offset:b}}(D))}),onOff:function(D){return _(E(d))(o.setOnOff({id:s,onOff:D}))}})(c)})(e)))))()}})}};return new P(a)}}},Hs=function(t){return wP(t)};function ZD(){window.scrollTo(0,0)}var bo=function(t){return t.sequential},Mn=function(t){return t.parallel};var kn=function(t){return function(r){return function(e){return new P(j(t)("button")(r)(q(e)))}}};var Fa=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",o="Bind",i="Bracket",m="Fork",s="Sequential",c="Map",D="Apply",b="Alt",f="Cons",h="Resume",et="Release",mt="Finalizer",ur="Finalized",jt="Forked",jr="Fiber",Qr="Thunk";function ct(zt,Kr,He,le){this.tag=zt,this._1=Kr,this._2=He,this._3=le}function _r(zt){var Kr=function(He,le,ir){return new ct(zt,He,le,ir)};return Kr.tag=zt,Kr}function hr(zt){return new ct(r,void 0)}function nr(zt){try{zt()}catch(Kr){setTimeout(function(){throw Kr},0)}}function qe(zt,Kr,He){try{return Kr(He())}catch(le){return zt(le)}}function Co(zt,Kr,He){try{return Kr(He)()}catch(le){return He(zt(le))(),hr}}var zu=function(){var zt=1024,Kr=0,He=0,le=new Array(zt),ir=!1;function Et(){var ee;for(ir=!0;Kr!==0;)Kr--,ee=le[He],le[He]=void 0,He=(He+1)%zt,ee();ir=!1}return{isDraining:function(){return ir},enqueue:function(ee){var wr,Le;Kr===zt&&(Le=ir,Et(),ir=Le),le[(He+Kr)%zt]=ee,Kr++,ir||Et()}}}();function Mi(zt){var Kr={},He=0,le=0;return{register:function(ir){var Et=He++;ir.onComplete({rethrow:!0,handler:function(ee){return function(){le--,delete Kr[Et]}}})(),Kr[Et]=ir,le++},isEmpty:function(){return le===0},killAll:function(ir,Et){return function(){if(le===0)return Et();var ee=0,wr={};function Le(de){wr[de]=Kr[de].kill(ir,function(Ze){return function(){delete wr[de],ee--,zt.isLeft(Ze)&&zt.fromLeft(Ze)&&setTimeout(function(){throw zt.fromLeft(Ze)},0),ee===0&&Et()}})()}for(var Xn in Kr)Kr.hasOwnProperty(Xn)&&(ee++,Le(Xn));return Kr={},He=0,le=0,function(de){return new ct(a,function(){for(var Ze in wr)wr.hasOwnProperty(Ze)&&wr[Ze]()})}}}}}var _u=0,cn=1,Qo=2,Xf=3,Qf=4,Oe=5,Ko=6;function Kf(zt,Kr,He){var le=0,ir=_u,Et=He,ee=null,wr=null,Le=null,Xn=null,de=null,Ze=0,vf=0,Su=null,wi=!0;function Pi(fr){for(var pr,qr,Xr;;)switch(pr=null,qr=null,Xr=null,ir){case Qo:ir=cn;try{Et=Le(Et),Xn===null?Le=null:(Le=Xn._1,Xn=Xn._2)}catch(fa){ir=Oe,ee=zt.left(fa),Et=null}break;case Xf:zt.isLeft(Et)?(ir=Oe,ee=Et,Et=null):Le===null?ir=Oe:(ir=Qo,Et=zt.fromRight(Et));break;case cn:switch(Et.tag){case o:Le&&(Xn=new ct(f,Le,Xn)),Le=Et._2,ir=cn,Et=Et._1;break;case r:Le===null?(ir=Oe,Et=zt.right(Et._1)):(ir=Qo,Et=Et._1);break;case a:ir=Xf,Et=qe(zt.left,zt.right,Et._1);break;case u:ir=Qf,Et=Co(zt.left,Et._1,function(fa){return function(){le===fr&&(le++,zu.enqueue(function(){le===fr+1&&(ir=Xf,Et=fa,Pi(le))}))}});return;case e:ir=Oe,ee=zt.left(Et._1),Et=null;break;case n:Le===null?de=new ct(f,Et,de,wr):de=new ct(f,Et,new ct(f,new ct(h,Le,Xn),de,wr),wr),Le=null,Xn=null,ir=cn,Et=Et._1;break;case i:Ze++,Le===null?de=new ct(f,Et,de,wr):de=new ct(f,Et,new ct(f,new ct(h,Le,Xn),de,wr),wr),Le=null,Xn=null,ir=cn,Et=Et._1;break;case m:ir=Xf,pr=Kf(zt,Kr,Et._2),Kr&&Kr.register(pr),Et._1&&pr.run(),Et=zt.right(pr);break;case s:ir=cn,Et=ST(zt,Kr,Et._1);break}break;case Oe:if(Le=null,Xn=null,de===null)ir=Ko,Et=wr||ee||Et;else switch(pr=de._3,Xr=de._1,de=de._2,Xr.tag){case n:wr&&wr!==pr&&Ze===0?ir=Oe:ee&&(ir=cn,Et=Xr._2(zt.fromLeft(ee)),ee=null);break;case h:wr&&wr!==pr&&Ze===0||ee?ir=Oe:(Le=Xr._1,Xn=Xr._2,ir=Qo,Et=zt.fromRight(Et));break;case i:Ze--,ee===null&&(qr=zt.fromRight(Et),de=new ct(f,new ct(et,Xr._2,qr),de,pr),(wr===pr||Ze>0)&&(ir=cn,Et=Xr._3(qr)));break;case et:de=new ct(f,new ct(ur,Et,ee),de,wr),ir=cn,wr&&wr!==pr&&Ze===0?Et=Xr._1.killed(zt.fromLeft(wr))(Xr._2):ee?Et=Xr._1.failed(zt.fromLeft(ee))(Xr._2):Et=Xr._1.completed(zt.fromRight(Et))(Xr._2),ee=null,Ze++;break;case mt:Ze++,de=new ct(f,new ct(ur,Et,ee),de,wr),ir=cn,Et=Xr._1;break;case ur:Ze--,ir=Oe,Et=Xr._1,ee=Xr._2;break}break;case Ko:for(var Ge in Su)Su.hasOwnProperty(Ge)&&(wi=wi&&Su[Ge].rethrow,nr(Su[Ge].handler(Et)));Su=null,wr&&ee?setTimeout(function(){throw zt.fromLeft(ee)},0):zt.isLeft(Et)&&wi&&setTimeout(function(){if(wi)throw zt.fromLeft(Et)},0);return;case _u:ir=cn;break;case Qf:return}}function ze(fr){return function(){if(ir===Ko)return wi=wi&&fr.rethrow,fr.handler(Et)(),function(){};var pr=vf++;return Su=Su||{},Su[pr]=fr,function(){Su!==null&&delete Su[pr]}}}function Dr(fr,pr){return function(){if(ir===Ko)return pr(zt.right(void 0))(),function(){};var qr=ze({rethrow:!1,handler:function(){return pr(zt.right(void 0))}})();switch(ir){case _u:wr=zt.left(fr),ir=Ko,Et=wr,Pi(le);break;case Qf:wr===null&&(wr=zt.left(fr)),Ze===0&&(ir===Qf&&(de=new ct(f,new ct(mt,Et(fr)),de,wr)),ir=Oe,Et=null,ee=null,Pi(++le));break;default:wr===null&&(wr=zt.left(fr)),Ze===0&&(ir=Oe,Et=null,ee=null)}return qr}}function Ir(fr){return function(){var pr=ze({rethrow:!1,handler:fr})();return ir===_u&&Pi(le),pr}}return{kill:Dr,join:Ir,onComplete:ze,isSuspended:function(){return ir===_u},run:function(){ir===_u&&(zu.isDraining()?Pi(le):zu.enqueue(function(){Pi(le)}))}}}function Yo(zt,Kr,He,le){var ir=0,Et={},ee=0,wr={},Le=new Error("[ParAff] Early exit"),Xn=null,de=t;function Ze(ze,Dr,Ir){var fr=Dr,pr=null,qr=null,Xr=0,Ge={},fa,dl;t:for(;;)switch(fa=null,fr.tag){case jt:if(fr._3===t&&(fa=Et[fr._1],Ge[Xr++]=fa.kill(ze,function(xT){return function(){Xr--,Xr===0&&Ir(xT)()}})),pr===null)break t;fr=pr._2,qr===null?pr=null:(pr=qr._1,qr=qr._2);break;case c:fr=fr._2;break;case D:case b:pr&&(qr=new ct(f,pr,qr)),pr=fr,fr=fr._1;break}if(Xr===0)Ir(zt.right(void 0))();else for(dl=0,fa=Xr;dl<fa;dl++)Ge[dl]=Ge[dl]();return Ge}function vf(ze,Dr,Ir){var fr,pr,qr,Xr,Ge,fa;zt.isLeft(ze)?(fr=ze,pr=null):(pr=ze,fr=null);t:for(;;){if(qr=null,Xr=null,Ge=null,fa=null,Xn!==null)return;if(Dr===null){le(fr||pr)();return}if(Dr._3!==t)return;switch(Dr.tag){case c:fr===null?(Dr._3=zt.right(Dr._1(zt.fromRight(pr))),pr=Dr._3):Dr._3=fr;break;case D:if(qr=Dr._1._3,Xr=Dr._2._3,fr){if(Dr._3=fr,Ge=!0,fa=ee++,wr[fa]=Ze(Le,fr===qr?Dr._2:Dr._1,function(){return function(){delete wr[fa],Ge?Ge=!1:Ir===null?vf(fr,null,null):vf(fr,Ir._1,Ir._2)}}),Ge){Ge=!1;return}}else{if(qr===t||Xr===t)return;pr=zt.right(zt.fromRight(qr)(zt.fromRight(Xr))),Dr._3=pr}break;case b:if(qr=Dr._1._3,Xr=Dr._2._3,qr===t&&zt.isLeft(Xr)||Xr===t&&zt.isLeft(qr))return;if(qr!==t&&zt.isLeft(qr)&&Xr!==t&&zt.isLeft(Xr))fr=pr===qr?Xr:qr,pr=null,Dr._3=fr;else if(Dr._3=pr,Ge=!0,fa=ee++,wr[fa]=Ze(Le,pr===qr?Dr._2:Dr._1,function(){return function(){delete wr[fa],Ge?Ge=!1:Ir===null?vf(pr,null,null):vf(pr,Ir._1,Ir._2)}}),Ge){Ge=!1;return}break}Ir===null?Dr=null:(Dr=Ir._1,Ir=Ir._2)}}function Su(ze){return function(Dr){return function(){delete Et[ze._1],ze._3=Dr,vf(Dr,ze._2._1,ze._2._2)}}}function wi(){var ze=cn,Dr=He,Ir=null,fr=null,pr,qr;t:for(;;)switch(pr=null,qr=null,ze){case cn:switch(Dr.tag){case c:Ir&&(fr=new ct(f,Ir,fr)),Ir=new ct(c,Dr._1,t,t),Dr=Dr._2;break;case D:Ir&&(fr=new ct(f,Ir,fr)),Ir=new ct(D,t,Dr._2,t),Dr=Dr._1;break;case b:Ir&&(fr=new ct(f,Ir,fr)),Ir=new ct(b,t,Dr._2,t),Dr=Dr._1;break;default:qr=ir++,ze=Oe,pr=Dr,Dr=new ct(jt,qr,new ct(f,Ir,fr),t),pr=Kf(zt,Kr,pr),pr.onComplete({rethrow:!1,handler:Su(Dr)})(),Et[qr]=pr,Kr&&Kr.register(pr)}break;case Oe:if(Ir===null)break t;Ir._1===t?(Ir._1=Dr,ze=cn,Dr=Ir._2,Ir._2=t):(Ir._2=Dr,Dr=Ir,fr===null?Ir=null:(Ir=fr._1,fr=fr._2))}for(de=Dr,qr=0;qr<ir;qr++)Et[qr].run()}function Pi(ze,Dr){Xn=zt.left(ze);var Ir;for(var fr in wr)if(wr.hasOwnProperty(fr)){Ir=wr[fr];for(fr in Ir)Ir.hasOwnProperty(fr)&&Ir[fr]()}wr=null;var pr=Ze(ze,de,Dr);return function(qr){return new ct(u,function(Xr){return function(){for(var Ge in pr)pr.hasOwnProperty(Ge)&&pr[Ge]();return hr}})}}return wi(),function(ze){return new ct(u,function(Dr){return function(){return Pi(ze,Dr)}})}}function ST(zt,Kr,He){return new ct(u,function(le){return function(){return Yo(zt,Kr,He,le)}})}return ct.EMPTY=t,ct.Pure=_r(r),ct.Throw=_r(e),ct.Catch=_r(n),ct.Sync=_r(a),ct.Async=_r(u),ct.Bind=_r(o),ct.Bracket=_r(i),ct.Fork=_r(m),ct.Seq=_r(s),ct.ParMap=_r(c),ct.ParApply=_r(D),ct.ParAlt=_r(b),ct.Fiber=Kf,ct.Supervisor=Mi,ct.Scheduler=zu,ct.nonCanceler=hr,ct}(),RE=Fa.Pure,WP=Fa.Throw;function NE(t){return function(r){return r.tag===Fa.Pure.tag?Fa.Pure(t(r._1)):Fa.Bind(r,function(e){return Fa.Pure(t(e))})}}function LE(t){return function(r){return Fa.Bind(t,r)}}var BE=Fa.Sync;function UE(t){return function(r){return Fa.ParMap(t,r)}}function WE(t){return function(r){return Fa.ParApply(t,r)}}function qE(t){return function(r){return Fa.ParAlt(t,r)}}var Kc=Fa.Async;function HE(t,r){return function(){return Fa.Fiber(t,null,r)}}var qP=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return Fa.Async(function(a){return function(){var u=t(n,a(e()));return function(){return Fa.Sync(function(){return e(r(n,u))})}}})}}(),zE=Fa.Seq;var zP=function(t){return function(r){return function(e){var n=bo(t),a=ae(t.Applicative1())(r)(function(){var u=Mn(t);return function(o){return u(e(o))}}());return function(u){return n(a(u))}}}},GE=function(t){return function(r){return function(e){var n=bo(t),a=Bn(r)(t.Applicative1())(function(){var u=Mn(t);return function(o){return u(e(o))}}());return function(u){return n(a(u))}}}},VE=function(t){return function(r){return zP(t)(r)(Z(tt))}};var GP=function(t){return t};var jE=function(t){return t};var W_=function(t){return t.toDuration};var XE={fromDuration:Nm()()(GP)(function(t){return t*1e3}),toDuration:Nm()()(jE)(function(t){return t/1e3})};var QE=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var JP=function(t){return t};var Zc={map:UE},ki={map:NE};var jP=function(){var t=function(n){if(n instanceof Qt)return n.value0;if(n instanceof Xt)return Ka("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof Xt)return n.value0;if(n instanceof Qt)return Ka("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof Xt)return!0;if(n instanceof Qt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:Xt.create,right:Qt.create}}(),XP=function(t){return HE(jP,t)},yo=function(t){return function(){var e=XP(t)();return e.run(),e}},Go=function(){var t=yr(F);return function(r){return t(yo(r))}}();var gi={apply:WE,Functor0:function(){return Zc}};var td={Applicative0:function(){return da},Bind1:function(){return Re}},Re={bind:LE,Apply0:function(){return rd(0)}},da={pure:RE,Apply0:function(){return rd(0)}},rd=QE("applyAff","Effect.Aff",function(){return{apply:Ju(td),Functor0:function(){return ki}}}),KE=rd(71);var Ie={liftEffect:BE,Monad0:function(){return td}},YE=function(){var t=De(Ie);return function(r){return JP(x(t(r)))}}(),ZE=function(t){return Kc(function(r){return p(F)(YE)(t.join(r))})};var t0=function(t){return function(r){return W(Re)(De(Ie)(r.isSuspended))(function(e){return e?De(Ie)(yr(F)(r.kill(t,x(_(l)(void 0))))):Kc(function(n){return p(F)(YE)(r.kill(t,n))})})}};var wn={parallel:_t,sequential:zE,Monad0:function(){return td},Applicative1:function(){return QP(0)}},QP=QE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Mn(wn),r=_(da);return function(e){return t(r(e))}}(),Apply0:function(){return gi}}});var KP={append:function(t){return function(r){return function(e){return VE(wn)(Nt)([t(e),r(e)])}}}};var YP=x(_(da)(void 0)),r0={mempty:YP,Semigroup0:function(){return KP}};var e0={alt:qE,Functor0:function(){return Zc}};var n0=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),q_=function(){function t(){}return t.value=new t,t}(),zf=function(){function t(){}return t.value=new t,t}(),H_=function(){function t(){}return t.value=new t,t}(),Gf=function(){function t(){}return t.value=new t,t}(),z_=function(){function t(){}return t.value=new t,t}(),G_=function(){function t(){}return t.value=new t,t}(),a0=function(){function t(){}return t.value=new t,t}(),zs=function(){function t(){}return t.value=new t,t}(),Gs=function(){function t(){}return t.value=new t,t}(),V_=function(){function t(){}return t.value=new t,t}(),J_=function(){function t(){}return t.value=new t,t}(),u0=function(){function t(){}return t.value=new t,t}(),tl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ed=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var ZP="numeric",tI="sudden",rI="unit",eI="cancel",nI="step",aI="linear",uI="exponential",oI="envelope",o0=function(t,r,e,n){if(e.type===tI)t.value=e.value.n;else if(e.type===rI)r.id&&fI(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===ZP)t[e.value.t.type===nI?"setValueAtTime":e.value.t.type===aI?"linearRampToValueAtTime":e.value.t.type===uI?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===eI)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===oI){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},iI=function(t,r,e,n,a){return n[e]||(n[e]={}),o0(r.parameters.get(e),n[e],a,t)},Wu=function(t,r,e,n,a){return n[e]||(n[e]={}),o0(r[e],n[e],a,t)},Ee=function(t,r,e){let n=r.value0?r.value0:"@fan@";e.scopes[n]||(e.scopes[n]=[]),e.scopes[n].push(t),e.units[t].scope=n},he=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},Te=function(t,r,e,n){t()(a=>i0(r,a,n))(e)},i0=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function nd(t){return function(r){return function(){delete r.units[t.id]}}}function ad(t){return function(r){return function(){i0(t.from,t.to,r)}}}var fI=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function ud(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var od=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},id=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),o=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:o,main:e.context.createGain(),se:u},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},fd=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},cd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},ld=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new ConstantSourceNode(o,i)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},_d=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},pd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},sd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},md=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},vd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Dd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},dd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},bd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new AudioBufferSourceNode(o,i)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},yd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Ad=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},kd=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var o=e.context.createMediaElementSource(a);return o};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},gd=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Cd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Ed=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},hd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){var m={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:cb(e.context)(i.spec.value.real)(i.spec.value.img)()},s=new OscillatorNode(o,m);return s},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Td=t=>r=>e=>()=>{var n=r.id,a=function(o,i){var m={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(o,m)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(o=>o)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Sd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),o=new MediaRecorder(u.stream);a(o)(),o.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:o,main:e.context.createGain(),se:u},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},xd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Fd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Od=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},$d=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Md=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},wd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)},Pd=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},Ee(n,r.scope,e),he(n,e),Te(t,n,r.parent,e)};function Id(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function Rd(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function Nd(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function Ld(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;iI(r,r.units[e].main,n,r.units[e].controllers,a)}}}var qu=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function Bd(t){return function(r){return function(){var e=t.id,n=t.gain;Wu(r,r.units[e].main,"gain",r.units[e].controllers,n),qu(n,r.units[e],"gain")}}}function Ud(t){return function(r){return function(){var e=t.id,n=t.q;Wu(r,r.units[e].main,"Q",r.units[e].controllers,n),qu(n,r.units[e],"Q")}}}function Wd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function qd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function Hd(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function zd(t){return function(r){return function(){var e=t.id,n=t.pan;Wu(r,r.units[e].main,"pan",r.units[e].controllers,n),qu(n,r.units[e],"pan")}}}function Gd(t){return function(r){return function(){var e=t.id,n=t.threshold;Wu(r,r.units[e].main,"threshold",r.units[e].controllers,n),qu(n,r.units[e],"threshold")}}}function Vd(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function Jd(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function jd(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function Xd(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function Qd(t){return function(r){return function(){var e=t.id,n=t.release;Wu(r,r.units[e].main,"release",r.units[e].controllers,n),qu(n,r.units[e],"release")}}}function Kd(t){return function(r){return function(){var e=t.id,n=t.offset;Wu(r,r.units[e].main,"offset",r.units[e].controllers,n),qu(n,r.units[e],"offset")}}}function Yd(t){return function(r){return function(){var e=t.id,n=t.ratio;Wu(r,r.units[e].main,"ratio",r.units[e].controllers,n),qu(n,r.units[e],"ratio")}}}function Zd(t){return function(r){return function(){var e=t.id,n=t.attack;Wu(r,r.units[e].main,"attack",r.units[e].controllers,n),qu(n,r.units[e],"attack")}}}function tb(t){return function(r){return function(){var e=t.id,n=t.knee;Wu(r,r.units[e].main,"knee",r.units[e].controllers,n),qu(n,r.units[e],"knee")}}}function rb(t){return function(r){return function(){var e=t.id,n=t.delayTime;Wu(r,r.units[e].main,"delayTime",r.units[e].controllers,n),qu(n,r.units[e],"delayTime")}}}function eb(t){return function(r){return function(){var e=t.id,n=t.playbackRate;Wu(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),qu(n,r.units[e],"playbackRate")}}}function nb(t){return function(r){return function(){var e=t.id,n=t.frequency;Wu(r,r.units[e].main,"frequency",r.units[e].controllers,n),qu(n,r.units[e],"frequency")}}}function ab(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?cI(e)(n)(r)():n.x.type==="off"&&lI(e)(n)(r)()}}}var cI=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},lI=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function ub(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function Vs(t){return function(){t.stop()}}function ob(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function ib(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function j_(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function fb(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var cb=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function uf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function lb(t){return function(){t.close()}}function _b(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function pb(t){return function(r){return function(){return t.decodeAudioData(r)}}}function sb(){return new(window.AudioContext||window.webkitAudioContext)}function mb(t){return function(){return t.state}}function X_(t){return function(){return t.currentTime}}function f0(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var sI=function(t){return function(r){return Kc(function(e){return Zf(F)($t(r0))(f0(r)(function(n){return e(Xt.create(t(n)))()})(function(n){return e(Qt.create(n))()}))})}};var mI=function(t){return Ia(function(r){return Bo("Promise failed, couldn't extract JS Error or String")})(Z(tt))(DD(N(vD(tv)(Xu))(CD(Xu)("Error")(t))(p(h_(ho))(Bo)(ED(Xu)(t)))))},c0=sI(mI),Js=function(t){return W(Re)(De(Ie)(t))(c0)};function vb(t){return function(){return URL.createObjectURL(t)}}var l0=function(t){return function(r){return function(e){return St(ob(t))(e)(function(){var n=Kn(Tn)(r);return function(a){return n(vb(a))}}())}}};var Vf={ids:p(F)(Gt(ip))(Bu),deleteFromCache:nd,disconnectXFromY:ud,connectXToY:ad,makeAllpass:od(Zt),makeAnalyser:id(Zt),makeAudioWorkletNode:fd(Zt),makeBandpass:cd(Zt),makeConstant:ld(Zt),makeConvolver:_d(Zt),makeDelay:pd(Zt),makeDynamicsCompressor:sd(Zt),makeGain:md(Zt),makeHighpass:vd(Zt),makeHighshelf:Dd(Zt),makeIIRFilter:dd(Zt),makeLoopBuf:bd(Zt),makeLowpass:yd(Zt),makeLowshelf:Ad(Zt),makeMediaElement:kd(Zt),makeMicrophone:gd(Zt),makeNotch:Cd(Zt),makePeaking:Ed(Zt),makePeriodicOsc:hd(Zt),makePlayBuf:Td(Zt),makeRecorder:Sd(Zt),makeSawtoothOsc:xd(Zt),makeSinOsc:Fd(Zt),makeSpeaker:Od,makeSquareOsc:Md(Zt),makeStereoPanner:$d(Zt),makeTriangleOsc:wd(Zt),makeWaveShaper:Pd(Zt),setAnalyserNodeCb:Id,setMediaRecorderCb:Rd,setWaveShaperCurve:Nd,setAudioWorkletParameter:Ld,setBuffer:Wd,setConvolverBuffer:qd,setDuration:Xd(Zt),setPeriodicOsc:Hd,setOnOff:ab,setBufferOffset:jd,setLoopStart:Vd,setLoopEnd:Jd,setRatio:Yd,setOffset:Kd,setAttack:Zd,setGain:Bd,setQ:Ud,setPan:zd,setThreshold:Gd,setRelease:Qd,setKnee:tb,setDelay:rb,setPlaybackRate:eb,setFrequency:nb},Ct=function(t){return function(r){return W(Re)(Js(_b(r)))(function(){var e=pb(t);return function(n){return Js(e(n))}}())}},Q_=function(t){var r=De(t);return function(e){return r(mb(e))}};var oa=function(t){return De(t)(sb)},Hu=function(t){var r=De(t);return function(e){return r(fb(e))}},gn=function(t){return function(r){return De(t)(function(){var n=Q_(fe)(r)();return In(l)(n!=="closed")(lb(r))()})}},yI=_t,AI=_t,js=function(t){return function(r){return p(ki)(function(e){return{microphone:function(){return t?_(Eo)(yI(e)):z.value}(),camera:function(){return r?_(Eo)(AI(e)):z.value}()}})(Js(ib(t)(r)))}};var Vo=function(){function t(){}return t.value=new t,t}(),Jo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Tu=function(){function t(){}return t.value=new t,t}(),fn=ZD,Ci=function(t){return bo(wn)(N(e0)(Mn(wn)(W(Re)(ZE(t))(De(Ie))))(Mn(wn)(t0(Bo("We navigated away from the page"))(t))))},rl=function(t){return function(r){return function(e){return function(n){return N(t)(_(r)(Tu.value))(n)}}}},Oa=function(t){return function(r){return function(e){return function(n){return N(t)(_(r)(Y(ve)(se.value)(Zr(x(n)))))(p(t.Functor0())(function(a){return Y(ve)(se.value)(Zr(x(X(at)(a)(n))))})(p(t.Functor0())(function(a){return a.value0})(e)))}}}},Xs=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(m){return p(t)(function(s){return Y(ve)(se.value)(Zr(x(function(){if(s.value0 instanceof Vo)return _(l)(void 0);if(s.value0 instanceof Jo)return X(at)(X(at)(s.value0.value0)(n(_(l)(void 0))))(a(Tu.value));if(s.value0 instanceof Tu)return function(){s.value1(),a(Vo.value)();var D=yo(W(Re)(oa(Ie))(function(b){return W(Re)(Hu(Ie)(b))(function(f){return W(Re)(u(b))(function(h){return De(Ie)(function(){var mt=o(b)(h)(),ur=X(at)(X(at)(mt)(f))(gn(fe)(b));return a(new Jo(ur))(),ur})})})}))();return te(r)(Tn)(n(function(){return a(Tu.value)(),Go(Ci(D))()}))(function(){return _(l)(void 0)})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[s.value0.constructor.name])}())))})(Fn(e)(N(e.Alternative0().Plus1().Alt0())(_(e.Alternative0().Applicative0())(_(l)(void 0)))(p(t)(function(s){return s.value0})(i)))(p(t)(ut.create)(m)))}}}}}}}}},$a=function(t){return function(r){return function(e){return function(){return t(e)(),r(new n0(e))()}}}},Qs=function(t){return function(r){return function(e){return function(n){return function(a){return Be(d)(function(u){return function(o){var i=rl(L(l))(E(d))(r)(o);return Tc(v)(N(L(l))(_(E(d))(Y(Kp)(Vt.value)("cursor: pointer;")))(Xs(C)(Gr)(Lt(d))(e)(u)(n)(a)(r)(i)))([nn(d)(p(C)(function(m){if(m instanceof Tu)return t;if(m instanceof Vo)return"\u23F3";if(m instanceof Jo)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[m.constructor.name])})(i))])}})}}}}},ht=function(t){return function(r){return function(e){return function(n){return Be(d)(function(a){return function(u){var o=rl(L(l))(E(d))(t)(u);return kn(v)(Xs(C)(Gr)(Lt(d))(r)(a)(e)(n)(t)(o))([nn(d)(p(C)(function(i){if(i instanceof Tu)return"Turn on";if(i instanceof Vo)return"Loading...";if(i instanceof Jo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[i.constructor.name])})(o))])}})}}}};var el=function(t){return function(r){return function(){var n=uf(t)(),a=xt(qf([new Sf(p(C)(function(u){return Tf.create(ek(u))})(r))])(Vf))(function(u){return u(n)})();return a}}};var vt=function(t){return function(r){return function(){var n=uf(t)(),a=xt(qf(r)(Vf))(function(u){return u(n)})();return a}}},Ks=function(t){return function(){var e=oa(fe)();return p(F)(function(n){return X(at)(n)(gn(fe)(e))})(vt(e)(t))()}};var kI=function(){return y.value}(),_0=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(v))(y.value)(kI)({allpass:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([xa(lr(gr)(a)(pt()))(function(u){return function(o){return Mt(it)(.2)([u,L_(jD)(700)([L_(Rs(gt(kt()(J(J(At)(ID)()()()({reflectSymbol:function(){return"q"}}))(Ps)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:990,q:20})([u]),L_(jD)(1110)([u,L_(Rs(gt(kt()(J(J(At)(ID)()()()({reflectSymbol:function(){return"q"}}))(Ps)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2010,q:30})([u])])])])}})])}})))})}}};function Ei(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,o=t.BYTES_PER_ELEMENT,i=Math.min(u,n>>>0);if(a===null)return new t(e,i);var m=Math.min((u-i)/o,a);return new t(e,i,m)}}var CI=Ei(Uint8ClampedArray),EI=Ei(Uint32Array),hI=Ei(Uint16Array),p0=Ei(Uint8Array),TI=Ei(Int32Array),SI=Ei(Int16Array),xI=Ei(Int8Array),FI=Ei(Float32Array),OI=Ei(Float64Array);function s0(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var Ys={create:p0,BinaryValue0:function(){}};var Zs=function(t){return function(r){return function(){return s0(r)}}};var nl=Au,al=Au,ul=Au,au=Au,uu=Au,ou=Au,iu=Au,fu=Au;function tm(t){return t|0}var hi=function(){return window};function d0(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var o=Object.getPrototypeOf(u),i=o.constructor.name;if(i===e)return r(n);if(i==="Object")return t;u=o}return t}var wt=function(t){return function(r){return d0(z.value,R.create,t,r)}};var Db=wt("HTMLCanvasElement");function b0(t){return function(){return t.body}}var y0=function(){var t=p(F)(Ke);return function(r){return t(b0(r))}}();var A0=_t;function Jf(t){return function(){return t.valueAsNumber}}var ol=wt("HTMLInputElement");function bb(t){return function(){return t.document}}function rm(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var yb=_t;var dR=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},of=qt(function(t){return function(){var e=hi(),n=pe(!0)(),a=dR("fx","FRP.Event.Animate",function(){return yr(F)(St(rm)(e)(function(){var i=Pe(n)();return In(l)(i)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),Sn(!1)(n)}});var bR="background-color: rgb(150,30,10);",yR="background-color: rgb(130,60,10);",AR="background-color: rgb(80,90,10);",kR="background-color: rgb(10,130,10);",gR="background-color: rgb(10,100,0);",CR=As(ru)(function(t){return $r(Or(bs)()(qa)()(C_))(bR)($r(Or(ua)()(mn)()(qa))(yR)($r(Or(ku)()(vn)()(mn))(AR)($r(Or(gu)()(Dn)()(vn))(kR)($r(Or(Cu)()(Eu)()(Dn))(gR)(Uu)))))}),ER=function(t){return function(r){return function(e){return function(n){return Ws(Us(gt(kt()(J(J(At)(Bs)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(yt()())))({cb:n,fftSize:xs.value})([lr(r)(e)(pt())])}}}},hR=function(){return y.value}(),Pr="background-color: rgb(255,255,255,0.0);",Nr=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(m){return function(s){return function(c){return p(t)(function(D){var b=E_(r)()(E_(n)()(D)(m))(s);return b?Y(u)(Vt.value)(E_(r)()(E_(n)()(CR)(m))(s)):Y(u)(Vt.value)(Pr)})(c)}}}}}}}}}}},TR=function(){return 15/40}(),SR=function(){return 10/40}(),xR=function(){return 7/40}(),FR=function(){return 3/40}(),OR=function(){return 1/40}(),g0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(v))(v)(hR)({analyser:B(rt(Be(d)(function(n){return function(a){var u=Gl(du(l))(Z(tt))(a),o=rl(L(l))(E(d))(e)(function(m){return m.right}(u)),i=function(m){return m.left}(u);return Wr(v)([kn(v)(N(L(l))(_(E(d))(Y(Ec)(Vt.value)("cursor: pointer;")))(Xs(C)(Gr)(Lt(d))(t)(function(m){return n(Qt.create(m))})(function(m){return Ct(m)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(m){return function(s){return function(){var D=pe(z.value)(),b=uf(m)(),f=qf([ER(Ls)(gr)(s)(function(et){return function(){return Sn(new R(et))(D)(),Sn(z.value)(D)}})])(Vf),h=xt(N(L(l))(p(C)(Qt.create)(f))(p(C)(Xt.create)(of)))(function(et){if(et instanceof Qt)return et.value0(b);if(et instanceof Xt)return function(){var ur=Pe(D)();return Ln(l)(Jr)(ur)(function(jt){return function(){var Qr=j_(jt)(),ct=Zs(Ys)(Qr)(),_r=pe(0)(),hr=pe(0)(),nr=pe(0)(),qe=pe(0)(),Co=pe(0)(),zu=pe(0)(),Mi=pe(0)(),_u=pe(0)(),cn=pe(0)(),Qo=pe(0)(),Xf=function(Oe){if(Oe<32)return _r;if(Oe<64)return hr;if(Oe<96)return nr;if(Oe<128)return qe;if(Oe<168)return Co;if(Oe<160)return zu;if(Oe<224)return Mi;if(ne)return _u;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 144, column 45 - line 152, column 63): "+[Oe.constructor.name])};Sl(ct)(function(Oe){var Ko=tm(Oe);return function(){var Yo=Pe(Qo)();return gf(Lr(xu)(Ko))(cn)(),gf(Lr(xu)(Ko))(Xf(Yo))(),gf(Lr(xu)(1))(Qo)()}})();var Qf=Bn(Bg)(l)(function(Oe){return function(){var Kf=p(F)(zr)(Pe(Oe))(),Yo=p(F)(Ku(Tl)(Kf))(p(F)(zr)(Pe(cn)))();return $r(Or(bs)()(qa)()(C_))(Yo>TR)($r(Or(ua)()(mn)()(qa))(Yo>SR)($r(Or(ku)()(vn)()(mn))(Yo>xR)($r(Or(gu)()(Dn)()(vn))(Yo>FR)($r(Or(Cu)()(Eu)()(Dn))(Yo>OR)(Uu)))))}})($r(Or(Ig)()(cD)()(Lg))(_r)($r(Or(Rg)()(lD)()(cD))(hr)($r(Or(Ng)()(C_)()(lD))(nr)($r(Or(bs)()(qa)()(C_))(qe)($r(Or(ua)()(mn)()(qa))(Co)($r(Or(ku)()(vn)()(mn))(zu)($r(Or(gu)()(Dn)()(vn))(Mi)($r(Or(Cu)()(Eu)()(Dn))(_u)(Uu)))))))))();return n(new Xt(Qf))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 125, column 57 - line 163, column 57): "+[et.constructor.name])})();return function(){return h(),function(){var ur=Q_(fe)(m)();return In(l)(ur!=="closed")(gn(fe)(m))()}(),n(new Xt(As(ru)(x(As(Sa)(x(!1))))))()}}}})(e)(o)))([nn(d)(p(C)(function(m){if(m instanceof Tu)return"Turn on";if(m instanceof Vo)return"Loading...";if(m instanceof Jo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 176, column 31 - line 179, column 56): "+[m.constructor.name])})(o))]),Sr(v)(_(E(d))(Y(st)(Vt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(Fe)(vo)(st)(Da)(vo)(fu)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(Gn)(mo)(st)(Da)(mo)(iu)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(zn)(so)(st)(Da)(so)(ou)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(Hn)(po)(st)(Da)(po)(uu)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(qn)(_o)(st)(Da)(_o)(au)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(Sa)(lo)(st)(Da)(lo)(ul)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(Wo)(co)(st)(Da)(co)(al)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Fe)(Da)(Uo)(fo)(st)(Da)(fo)(nl)(fu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(Fe)(vo)(st)(va)(vo)(fu)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(Gn)(mo)(st)(va)(mo)(iu)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(zn)(so)(st)(va)(so)(ou)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(Hn)(po)(st)(va)(po)(uu)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(qn)(_o)(st)(va)(_o)(au)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(Sa)(lo)(st)(va)(lo)(ul)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(Wo)(co)(st)(va)(co)(al)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Gn)(va)(Uo)(fo)(st)(va)(fo)(nl)(iu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(Fe)(vo)(st)(ma)(vo)(fu)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(Gn)(mo)(st)(ma)(mo)(iu)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(zn)(so)(st)(ma)(so)(ou)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(Hn)(po)(st)(ma)(po)(uu)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(qn)(_o)(st)(ma)(_o)(au)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(Sa)(lo)(st)(ma)(lo)(ul)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(Wo)(co)(st)(ma)(co)(al)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(zn)(ma)(Uo)(fo)(st)(ma)(fo)(nl)(ou)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(Fe)(vo)(st)(sa)(vo)(fu)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(Gn)(mo)(st)(sa)(mo)(iu)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(zn)(so)(st)(sa)(so)(ou)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(Hn)(po)(st)(sa)(po)(uu)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(qn)(_o)(st)(sa)(_o)(au)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(Sa)(lo)(st)(sa)(lo)(ul)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(Wo)(co)(st)(sa)(co)(al)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(Hn)(sa)(Uo)(fo)(st)(sa)(fo)(nl)(uu)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(Fe)(vo)(st)(pa)(vo)(fu)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(Gn)(mo)(st)(pa)(mo)(iu)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(zn)(so)(st)(pa)(so)(ou)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(Hn)(po)(st)(pa)(po)(uu)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(qn)(_o)(st)(pa)(_o)(au)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(Sa)(lo)(st)(pa)(lo)(ul)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(Wo)(co)(st)(pa)(co)(al)(au)(i)))([]),Sr(v)(N(L(l))(_(E(d))(Y(st)(Vt.value)(Pr)))(Nr(C)(qn)(pa)(Uo)(fo)(st)(pa)(fo)(nl)(au)(i)))([])])])}})))})}}};var MR=function(){return y.value}(),C0=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(v))(y.value)(MR)({bandpass:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([xa(lr(gr)(a)(pt()))(function(u){return function(o){return Mt(it)(.8)([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:400,q:1})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:880,q:5})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:1200,q:10})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2e3,q:20})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var PR=function(){return y.value}(),E0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(v))(v)(PR)({compression:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([OE(dE(gt(kt()(At))(yt()())))({})([lr(gr)(a)(pt())])])}})))})}}};var ia=function(){return function(t){var r=rn(),e=ie()({reflectSymbol:function(){return"playbackRate"}})(y.value),n=Bf(t);return function(a){return r(e(n(a)))}}},jf=function(){return function(t){var r=rn(),e=ie()({reflectSymbol:function(){return"onOff"}})(y.value),n=TC(t);return function(a){return r(e(n(a)))}}},h0=function(){return function(t){var r=rn(),e=ie()({reflectSymbol:function(){return"offset"}})(y.value),n=Bf(t);return function(a){return r(e(n(a)))}}},T0=function(){var t=rn(),r=ie()({reflectSymbol:function(){return"loopStart"}})(y.value);return function(e){return t(r(e))}},S0=function(){var t=rn(),r=ie()({reflectSymbol:function(){return"loopEnd"}})(y.value);return function(e){return t(r(e))}},Cn=function(){return function(t){var r=rn(),e=ie()({reflectSymbol:function(){return"gain"}})(y.value),n=Bf(t);return function(a){return r(e(n(a)))}}},Ao=function(){return function(t){var r=rn(),e=ie()({reflectSymbol:function(){return"frequency"}})(y.value),n=Bf(t);return function(a){return r(e(n(a)))}}};var il=function(){return function(t){var r=rn(),e=ie()({reflectSymbol:function(){return"delayTime"}})(y.value),n=Bf(t);return function(a){return r(e(n(a)))}}};var RR=function(){return y.value}(),x0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(U()(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}})(v))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(v))(v)(RR)({tf:B(oe(d)("<|>")),txt:B(oe(d)(`run2_
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
  ]`)),constant:B(rt(ht(e)(t)(function(n){return _(da)(void 0)})(function(n){return function(a){return vt(n)([Mt(it)(.5)([Hs(ws)(0)(N(L(l))(pt())(_(E(d))(h0()($n)({d:5,o:.1,p:ro(ai)(function(u){return x(function(){var o=ja(Qu)(u)(3)===0;return o?1:0}())})(ln(0)(1920))}))))])])}})))})}}};var LR=function(){return y.value}(),F0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(v))(v)(LR)({convolution:B(rt(ht(e)(t)(function(n){return It(KE)(p(ki)(function(a){return function(u){return{loop:a,verb:u}}})(Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Ct(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return vt(n)([EE(UC)(a.verb)([lr(gr)(a.loop)(pt())])])}})))})}}};var UR=function(){return y.value}(),O0=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(v))(y.value)(UR)({delay:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return vt(n)([xa(jn(Ha)(a)(pt()))(function(u){return function(o){return Mt(it)(.2)([Do(Ye)(.03)([u]),Do(Ye)(.1)([u]),Do(Ye)(.3)([u]),Do(Ye)(.7)([u])])}})])}})))})}}};var qR=function(){return y.value}(),$0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(v))(v)(qR)({gain:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return vt(n)([Mt(it)(.1)([lr(gr)(a)(pt())])])}})))})}}};var zR=function(){return y.value}(),M0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(v))(v)(zR)({highpass:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Jc(eu)(2e3)([lr(gr)(a)(pt())])])}})))})}}};var VR=function(){return y.value}(),w0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(v))(v)(VR)({highshelf:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([$E(DE(gt(kt()(J(J(At)(JC)()()()({reflectSymbol:function(){return"gain"}}))(jC)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2e3,gain:.4})([lr(gr)(a)(pt())])])}})))})}}};var jR=function(){return y.value}(),P0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})(v))(v)(jR)({iirFilterEx:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([hE()()(BC(vi)(vi))(new ut(Ff()()(20298e-8)(Ff()()(.0004059599)(Ff()()(20298e-8)(kv))),Ff()()(1.0126964558)(Ff()()(-1.9991880801)(Ff()()(.9873035442)(kv)))))([lr(gr)(a)(pt())])])}})))})}}};var QR=function(){return y.value}(),I0=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(v))(y.value)(QR)({loopBuf:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return vt(n)([lr(Wf(gt(kt()(J(J(J(J(At)(zc)()()()({reflectSymbol:function(){return"playbackRate"}}))(w_)()()()({reflectSymbol:function(){return"loopStart"}}))(M_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Uf)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(pt()),lr(Wf(gt(kt()(J(J(J(J(At)(zc)()()()({reflectSymbol:function(){return"playbackRate"}}))(w_)()()()({reflectSymbol:function(){return"loopStart"}}))(M_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Uf)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(pt()),lr(Wf(gt(kt()(J(J(At)(zc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Uf)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())))({buffer:a,playbackRate:1.7})(pt())])}})))})}}};var YR=function(){return y.value}(),R0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(v))(v)(YR)({lowpass:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([jc(Is)(215)([lr(gr)(a)(pt())])])}})))})}}};var tN=function(){return y.value}(),N0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(v))(v)(tN)({lowshelf:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([wE(vE(gt(kt()(J(J(At)(zC)()()()({reflectSymbol:function(){return"gain"}}))(GC)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:91,gain:.4})([lr(gr)(a)(pt())])])}})))})}}};var eN=function(){return y.value}(),L0=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use \u{1F3A7} when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(v))(y.value)(eN)({microphone:B(rt(ht(e)(t)(function(n){return js(!0)(!1)})(function(n){return function(a){return vt(n)([function(){if(a.microphone instanceof R)return nu(function(u){return Mt(it)(1)([R_(O_)(a.microphone.value0),Do(Ye)(.1)([Mt(it)(.2)([u])])])});if(a.microphone instanceof z)return Mt(it)(.02)([xE(af)(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[a.microphone.constructor.name])}()])}})))})}}};var aN=function(){return y.value}(),B0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(v))(v)(aN)({notch:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Xc(Vc(gt(kt()(J(J(At)(qc)()()()({reflectSymbol:function(){return"q"}}))(Hc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:400,q:1})(_(_e)(Xc(Vc(gt(kt()(J(J(At)(qc)()()()({reflectSymbol:function(){return"q"}}))(Hc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:880,q:5})(_(_e)(Xc(Vc(gt(kt()(J(J(At)(qc)()()()({reflectSymbol:function(){return"q"}}))(Hc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:1200,q:10})(_(_e)(Xc(Vc(gt(kt()(J(J(At)(qc)()()()({reflectSymbol:function(){return"q"}}))(Hc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2e3,q:20})(_(_e)(Xc(Vc(gt(kt()(J(J(At)(qc)()()()({reflectSymbol:function(){return"q"}}))(Hc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:3e3,q:30})(_(_e)(lr(gr)(a)(pt())))))))))))])}})))})}}};var oN=function(){return y.value}(),U0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(v))(v)(oN)({peaking:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Qc(Gc(gt(kt()(J(J(J(At)(Bc)()()()({reflectSymbol:function(){return"q"}}))(Uc)()()()({reflectSymbol:function(){return"gain"}}))(Wc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:400,q:1,gain:-20})(_(_e)(Qc(Gc(gt(kt()(J(J(J(At)(Bc)()()()({reflectSymbol:function(){return"q"}}))(Uc)()()()({reflectSymbol:function(){return"gain"}}))(Wc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:880,q:5,gain:20})(_(_e)(Qc(Gc(gt(kt()(J(J(J(At)(Bc)()()()({reflectSymbol:function(){return"q"}}))(Uc)()()()({reflectSymbol:function(){return"gain"}}))(Wc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:1200,q:10,gain:-20})(_(_e)(Qc(Gc(gt(kt()(J(J(J(At)(Bc)()()()({reflectSymbol:function(){return"q"}}))(Uc)()()()({reflectSymbol:function(){return"gain"}}))(Wc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2e3,q:20,gain:20})(_(_e)(Qc(Gc(gt(kt()(J(J(J(At)(Bc)()()()({reflectSymbol:function(){return"q"}}))(Uc)()()()({reflectSymbol:function(){return"gain"}}))(Wc)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:3e3,q:30,gain:-20})(_(_e)(lr(gr)(a)(pt())))))))))))])}})))})}}};var fN=function(){return y.value}(),W0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(v))(v)(fN)({periodic:B(rt(ht(e)(t)(function(n){return _(da)(void 0)})(function(n){return function(a){return vt(n)([Mt(it)(.2)([Ai(yi(gt(kt()(J(J(At)(bi(Di(ua)))()()()({reflectSymbol:function(){return"spec"}}))(di)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:140,spec:new ut($r(Or(ua)()(mn)()(qa))(.1)($r(Or(ku)()(vn)()(mn))(.2)($r(Or(gu)()(Dn)()(vn))(.3)($r(Or(Cu)()(Eu)()(Dn))(.4)(Uu)))),$r(Or(ua)()(mn)()(qa))(.4)($r(Or(ku)()(vn)()(mn))(.3)($r(Or(gu)()(Dn)()(vn))(.2)($r(Or(Cu)()(Eu)()(Dn))(.1)(Uu)))))})(pt())])])}})))})}}};var lN=function(){return y.value}(),q0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(v))(v)(lN)({playBuf:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return vt(n)([jn(P_(gt(kt()(J(J(J(At)(qC)()()()({reflectSymbol:function(){return"duration"}}))(WC)()()()({reflectSymbol:function(){return"bufferOffset"}}))($_)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())))({buffer:a,duration:3,bufferOffset:4.2})(pt())])}})))})}}};var Ab=function(){function t(){}return t.value=new t,t}();var H0={attr:function(t){return function(r){return A({key:"controls",value:H(r)})}}};var kb=function(){function t(){}return t.value=new t,t}();var z0={attr:function(t){return function(r){return A({key:"src",value:H(r)})}}};var gb=function(t){return function(r){return function(e){return new P(j(t)("audio")(r)(q(e)))}}};var vN=function(t){return function(r){return function(e){return function(n){return QD(t)(n)(R_(r)(e))}}}},DN=function(){return y.value}(),G0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(v))(v)(DN)({recorder:B(rt(Be(d)(function(n){return function(a){var u=Gl(du(l))(Z(tt))(a),o=Gl(du(l))(Z(tt))(function(c){return c.left}(u)),i=function(c){return c.right}(o),m=rl(L(l))(E(d))(e)(function(c){return c.right}(u)),s=function(c){return c.left}(o);return Wr(v)([kn(v)(N(L(l))(_(E(d))(Y(Ec)(Vt.value)("cursor: pointer;")))(p(C)(function(c){return Y(ve)(se.value)(Zr(x(function(){if(c.e instanceof Vo)return _(l)(void 0);if(c.e instanceof Jo)return X(at)(X(at)(X(at)(c.e.value0)(t(_(l)(void 0))))(Ln(l)(Jr)(c.rec)(function(){var D=m_(Qv);return function(b){return D(Vs(b))}}())))(n(Qt.create(Tu.value)));if(c.e instanceof Tu)return function(){c.cncl();var b=I_();n(new Qt(Vo.value))();var f=yo(W(Re)(p(ki)(function(h){return h.microphone})(js(!0)(!1)))(function(h){return De(Ie)(function(){var mt=Zt(_(l)(_(l)(void 0)))(function(ur){return function(){var jr=oa(fe)(),Qr=uf(jr)(),ct=qf([vN($D)(O_)(ur)(function(hr){return function(){return n(new Xt(new Qt(hr)))(),yr(F)(Ns(hr)(b))(),l0("audio/ogg; codecs=opus")(function(qe){return n(Xt.create(Xt.create(qe)))})(hr)()}})])(Vf),_r=xt(ct)(function(hr){return hr(Qr)})();return function(){_r(),W(Tn)(gE(b))(ae(l)(Jr)(function(){var qe=m_(Qv);return function(Co){return qe(Vs(Co))}}()))();var nr=Q_(fe)(jr)();return In(l)(nr!=="closed")(gn(fe)(jr))()}}})(h)();return n(new Qt(new Jo(mt)))(),mt})}))();return t(function(){return n(Qt.create(Tu.value))(),Go(Ci(f))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 64, column 47 - line 107, column 52): "+[c.e.constructor.name])}())))})(Fn(Lt(d))(N(L(l))(_(E(d))(z.value))(p(C)(R.create)(i)))(p(C)(Yf)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(c){return c.value0})(e)))(p(C)(function(c){return function(D){return function(b){return{e:c,cncl:D,rec:b}}}})(m)))))))([nn(d)(p(C)(function(c){if(c instanceof Tu)return"Turn on";if(c instanceof Vo)return"Loading...";if(c instanceof Jo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 118, column 31 - line 121, column 56): "+[c.constructor.name])})(m))]),Wr(v)([gb(v)(N(L(l))(_(E(d))(Y(H0)(Ab.value)("true")))(N(L(l))(_(E(d))(Y(Iv)(Vt.value)("display:none;")))(N(L(l))(p(C)(function(c){return Y(z0)(kb.value)(c)})(s))(p(C)(x(Y(Iv)(Vt.value)("display:block;")))(s)))))([])])])}})))})}}};var bN=function(){return y.value}(),V0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(v))(v)(bN)({periodic:B(rt(ht(e)(t)(function(n){return _(da)(void 0)})(function(n){return function(a){return vt(n)([Mt(it)(.2)([SE(LC)(448)(pt())])])}})))})}}};var AN=function(){return y.value}(),J0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(v))(v)(AN)({periodic:B(rt(ht(e)(t)(function(n){return _(da)(void 0)})(function(n){return function(a){return vt(n)([Mt(it)(.2)([Hf(af)(448)(pt())])])}})))})}}};var gN=function(){return y.value}(),j0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(v))(v)(gN)({periodic:B(rt(ht(e)(t)(function(n){return _(da)(void 0)})(function(n){return function(a){return vt(n)([Mt(it)(.2)([N_(Lc)(448)(pt())])])}})))})}}};var EN=function(){return y.value}(),X0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(v))(v)(EN)({pan:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return vt(n)([PE(NC)(1)([lr(gr)(a)(pt())])])}})))})}}};var TN=function(){return y.value}(),Q0=Ft({reflectType:function(){return`<ul>
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
`}})()()(K(l))(v)(TN)({});var xN=function(){return y.value}(),K0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(U()(K(l))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(v))(v)(xN)({periodic:B(rt(ht(e)(t)(function(n){return _(da)(void 0)})(function(n){return function(a){return vt(n)([Mt(it)(.2)([qs(Ms)(448)(pt())])])}})))})}}};var ON=function(){return y.value}(),Y0=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(U()(U()(K(l))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(v))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(v))(v)(ON)({code:B(oe(d)(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(o){var i=rf/180;return p(Fr)(function(m){var s=zr(m)*2/zr(44100)-1;return(3+o)*s*20*i/(rf+o*km(Pa)(Df)(s))})(ln(0)(44099))};return vt(n)([IE(mE)(ub(u(400)))([lr(gr)(a)(pt())])])}})))})}}};var MN=function(){return y.value}(),Z0=function(t){return function(r){return function(e){return function(n){var a=X(at)(r(Gf.value))(fn),u=$a(t)(e);return Ft({reflectType:function(){return`<div>
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
</div>`}})()()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(an()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(U()(K(l))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(v))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}})(v))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}})(v))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}})(v))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}})(v))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}})(v))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(v))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(v))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}})(v))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(v))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(v))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(v))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(v))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(v))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(v))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(v))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}})(v))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(v))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(v))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(v))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}})(v))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(v))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(v))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(v))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(v))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(v))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(v))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(v))(v)(MN)({drumroll:B(rt(Qs("\u{1F941}")(n)(u)(function(o){return Ct(o)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(o){return function(i){return vt(o)([Mt(it)(1)([lr(gr)(i)(pt())])])}}))),toc:B(Q0),allpass:B(_0(u)(r)(n)),analyser:B(g0(u)(r)(n)),bandpass:B(C0(u)(r)(n)),constant:B(x0(u)(r)(n)),compression:B(E0(u)(r)(n)),convolution:B(F0(u)(r)(n)),delay:B(O0(u)(r)(n)),gain:B($0(u)(r)(n)),highpass:B(M0(u)(r)(n)),highshelf:B(w0(u)(r)(n)),iirFilter:B(P0(u)(r)(n)),loopBuf:B(I0(u)(r)(n)),lowshelf:B(N0(u)(r)(n)),lowpass:B(R0(u)(r)(n)),notch:B(B0(u)(r)(n)),playBuf:B(q0(u)(r)(n)),peaking:B(U0(u)(r)(n)),microphone:B(L0(u)(r)(n)),pan:B(X0(u)(r)(n)),periodicOsc:B(W0(u)(r)(n)),recorder:B(G0(u)(r)(n)),sawtoothOsc:B(V0(u)(r)(n)),sinOsc:B(J0(u)(r)(n)),squareOsc:B(j0(u)(r)(n)),triangleOsc:B(K0(u)(r)(n)),waveShaper:B(Y0(u)(r)(n)),next:Oa(L(l))(E(d))(n)(a)})}}}};var Cb=function(){function t(){}return t.value=new t,t}(),th={attr:function(t){return function(r){return A({key:"checked",value:H(r)})}}};var ko=function(){function t(){}return t.value=new t,t}();var jo={attr:function(t){return function(r){return A({key:"type",value:H(r)})}}};var go=function(t){return function(r){return function(e){return new P(j(t)("input")(r)(q(e)))}}};var RN=function(t){return t},um=function(t){return function(r){return function(e){return ci(t)(N(t.Alternative0().Plus1().Alt0())(_(t.Alternative0().Applicative0())(r))(e))}}};var Z_=function(t){return function(r){return t(r)}},ff=function(t){return{map:function(r){return function(e){return function(n){return e(p(t)(function(a){return function(u){return a(r(u))}})(n))}}}}},Ti=function(t){return function(r){return function(e){return function(n){return Z_(p(ff(t))(r)(e))(p(t)(Ri)(n))}}}};var fl=function(t){return Ti(t)(x)};var cu=RN;var rh=function(t){return function(r){return function(e){return cu(function(n){return xe(t)(N(t.Alternative0().Plus1().Alt0())(_(t.Alternative0().Applicative0())(Z_(r)(n)))(p(t.Filterable1().Functor1())(function(a){return Z_(a)(n)})(e)))})}}},Eb=function(t){return{apply:function(r){return function(e){return function(n){return e(r(p(t)($u(Zo))(n)))}}},Functor0:function(){return ff(t)}}};var cl=function(t){return function(r){return qt(function(e){return xt(r)(function(n){return function(){var u=X_(t)();return e({acTime:u,value:n})()}})})}};var eh=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(o){return function(i){return function(m){return function(s){return function(){var D=Pe(o)();return In(l)(D)(function(){var f=X_(t)(),h=Ip(Yk(Ou(Pa)(u-f-.04)(.01)*1e3))(function(){var mt=Pe(o)();return In(l)(mt)(function(){return Sn(u)(m)(),a(u)(),n(a)(u+s)(o)(i)(m)(s)()})()})();return Sn(new R(h))(i)()})()}}}}}}};return qt(function(a){return function(){var o=pe(!0)(),i=pe(z.value)(),m=X_(t)(),s=pe(m+r)();n(a)(r)(o)(i)(s)(r)();var c=xt(e)(function(D){return function(){W(Tn)(Pe(i))(ae(l)(Jr)(jl))();var f=Pe(s)();return n(a)(f+D)(o)(i)(s)(D)()}})();return X(at)(X(at)(c)(Sn(!1)(o)))(W(Tn)(Pe(i))(ae(l)(Jr)(jl)))}})}}};var Ma=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var o=(n-r)/(e-t),i=r-o*t;return o*a+i}}}}};var NN=function(){return y.value}(),nh=function(t){return function(r){return function(e){return function(n){return Ft({reflectType:function(){return`<section>
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

</section>`}})()()(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(v))(v)(NN)({txt:B(oe(d)(`module Main where

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
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
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
                            evs f a = sampleOn acTime
                              $ map ($)
                              $ sampleOn a
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
  )`)),empl:B(rt(tu()(d)(Ru({reflectSymbol:function(){return"cbx"}})()()()(ue({reflectSymbol:function(){return"cbx0"}})()()(ue({reflectSymbol:function(){return"cbx1"}})()()(ue({reflectSymbol:function(){return"cbx2"}})()()(ue({reflectSymbol:function(){return"cbx3"}})()()(Wn)()()()())()()()())()()()())()()()())(Ru({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(Wn)()()()())()()()())(y.value)(function(a){return function(u){var o=N(L(l))(_(E(d))(void 0))(u.startStop.start),i=function(b){return um(Lt(d))(!1)(Pu(Lt(d))(x(Qa(La)))(b)(!1))},m=i(u.cbx.cbx3),s=i(u.cbx.cbx2),c=i(u.cbx.cbx1),D=i(u.cbx.cbx0);return Wr(v)([kn(v)(_n(Nt)(S(l))(p(C)(function(){var b=Y(ve)(se.value);return function(f){return b(Zr(x(f)))}}()))([Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(b){return b.value0})(n)))(Q(C)(o)(Z(tt))))(function(b){return function(){b();var h=oa(fe)(),et=Hu(fe)(h)(),mt=function(jr){return function(Qr){return function(ct){return Xl(Lt(d))(function(_r){return function(hr){var nr=hr.value1+(_r.value1-hr.value0)*function(){return _r.value0?jr:1}();return new ut(new ut(_r.value1,nr),nr)}})(Ti(C)(ut.create)(Qr)(ct))(new ut(0,0))}}},ur=el(h)(Qi(d)(p(C)(function(){var jr=Lr(Aa)(.04);return function(Qr){return jr(function(ct){return ct.acTime}(Qr))}}())(cl(h)(of)))(function(jr){var Qr=function(qe){return function(Co){return ci(Lt(d))(jr)(p(C)(Yf)(ci(Lt(d))(Co)(p(C)(function(zu){return function(Mi){return function(_u){return{f:zu,a:Mi,t:_u}}}})(qe))))}},ct=p(C)(function(qe){return qe?4:1})(fl(C)(m)(jr)),_r=mt(4)(s)(jr),hr=p(C)(function(qe){return qe?4:1})(fl(C)(c)(jr)),nr=mt(8)(D)(jr);return[We(it)(0)(Yr(C)(Qr(nr)(hr))(function(qe){return Cn()(Ne)({n:Ma(1)(.01)(4)(.15)(qe.a)*ts(rf*qe.f)+.15,o:qe.t,t:qo})}))([Ai(yi(gt(kt()(J(J(At)(bi(Di(ua)))()()()({reflectSymbol:function(){return"spec"}}))(di)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:325.6,spec:new ut($r(Or(ua)()(mn)()(qa))(.3)($r(Or(ku)()(vn)()(mn))(-.1)($r(Or(gu)()(Dn)()(vn))(.7)($r(Or(Cu)()(Eu)()(Dn))(-.4)(Uu)))),$r(Or(ua)()(mn)()(qa))(.6)($r(Or(ku)()(vn)()(mn))(.3)($r(Or(gu)()(Dn)()(vn))(.2)($r(Or(Cu)()(Eu)()(Dn))(0)(Uu)))))})(Ce(Nt)(S(l))([pt(),Yr(C)(Qr(_r)(ct))(function(qe){return Ao()(Ne)({n:325.6+Ma(1)(3)(4)(15.5)(qe.a)*ts(rf*qe.f),o:qe.t,t:qo})})]))])]}))(),jt=X(at)(X(at)(ur)(et))(gn(fe)(h));return t(X(at)(jt)(a.startStop.start(void 0)))(),a.startStop.stop(jt)()}}),Yr(C)(u.startStop.stop)(function(b){return X(at)(b)(X(at)(t(_(l)(void 0)))(a.startStop.start(void 0)))})]))([nn(d)(Ce(Nt)(S(l))([Q(C)(o)("Turn on"),Q(C)(u.startStop.stop)("Turn off")]))]),Sr(v)(_n(Nt)(S(l))(p(C)(Y(st)(Vt.value)))([Q(C)(u.startStop.stop)("display:block;"),Q(C)(o)("display:none;")]))(p(Fr)(function(b){return go(v)(Ce(Nt)(S(l))([_(E(d))(Y(jo)(ko.value)("checkbox")),_(E(d))(Y(ve)(se.value)(Zr(x(b(void 0))))),Q(C)(o)(Y(th)(Cb.value)("false"))]))([])})(gm(Fr)([function(b){return b.cbx0},function(b){return b.cbx1},function(b){return b.cbx2},function(b){return b.cbx3}])(a.cbx)))])}})))})}}}};var hb={recip:function(t){return 1/t},Ring0:function(){return Df}};var Tb=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function ll(t){return function(){return function(r){return t(r)()}}}function _l(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function pl(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function Sb(t){return t.clientX}function xb(t){return t.clientY}function tp(t){return t.button}var rp=wt("MouseEvent");var ah=function(t){return function(r){return qt(function(e){return xt(r)(function(n){return function(){var u=Pe(t.buttons)();return e({value:n,buttons:u})()}})})}};var uh=function(){var r=pe(z.value)(),e=pe(lv)(),n=p(F)(yb)(hi)(),a=ll(function(m){return ae(l)(Jr)(function(s){return Sn(new R({x:Sb(s),y:xb(s)}))(r)})(rp(m))})(),u=ll(function(m){return ae(l)(Jr)(function(s){return nc(GA(Je)(tp(s)))(e)})(rp(m))})(),o=ll(function(m){return ae(l)(Jr)(function(s){return nc(Pp(Je)(tp(s)))(e)})(rp(m))})();_l(rn()("mousemove"))(a)(!1)(n)(),_l(rn()("mousedown"))(u)(!1)(n)(),_l(rn()("mouseup"))(o)(!1)(n)();var i=function(){return pl(rn()("mousemove"))(a)(!1)(n)(),pl(rn()("mousedown"))(u)(!1)(n)(),pl(rn()("mouseup"))(o)(!1)(n)()};return{position:r,buttons:e,dispose:i}},oh=qt(function(t){return function(){var e=p(F)(yb)(hi)(),n=ll(function(a){return ae(l)(Jr)(function(u){return t(tp(u))})(rp(a))})();return _l(rn()("mousedown"))(n)(!1)(e)(),pl(rn()("mousedown"))(n)(!1)(e)}});var fh=function(t){return cu(function(r){return p(C)(function(e){return e.value(e.buttons)})(ah(t)(r))})};var $b=function(t){return t};function fm(){return Date.now()}var wh=function(t){return qt(function(r){return xt(t)(function(e){return function(){var a=fm();return r({time:a,value:e})()}})})};var y1=cu(function(t){return p(C)(function(r){return r.value(r.time)})(wh(t))}),wb=p(ff(C))(function(){var t=W_(XE);return function(r){return t($b(r))}}())(y1);var k1=function(t){var r=function(u){return function(o){return function(i){return function(m){return function(s){return function(c){return function(D){var b=Lr(o.DivisionRing1().Ring0().Semiring0())(ka(o.DivisionRing1().Ring0().Semiring0()))(ka(o.DivisionRing1().Ring0().Semiring0())),f=function(h){return function(et){if(h.last instanceof z)return et;if(h.last instanceof R)return Lr(i)(et)(m(function(mt){return Ku(o.EuclideanRing0())(Pn(o.DivisionRing1().Ring0().Semiring0())(mt(Lr(i)(h.last.value0.value1)(h.now.value1)))(Fu(o.DivisionRing1().Ring0())(h.now.value0)(h.last.value0.value0)))(b)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 103, column 5 - line 103, column 35): "+[h.constructor.name,et.constructor.name])}};return cu(function(h){var et=Z_(D)(Q(u.Filterable1().Functor1())(h)(Z(tt))),mt=Np(u)(Ti(u.Filterable1().Functor1())(ut.create)(c)(et)),ur=Pu(u)(f)(mt)(s);return ci(u)(ur)(h)})}}}}}}},e=function(u){return function(o){return r(u)(o)(o.DivisionRing1().Ring0().Semiring0())(function(i){return i(Z(tt))})}},n=function(u){return function(o){return cu(function(i){return Ql(Lt(d))(function(m){var s=o(um(Lt(d))(u)(m));return{input:fl(C)(s)(i),output:ci(Lt(d))(m)(i)}})})}},a=function(u){return function(o){return function(i){if(zA(u))return-8*(o-1)-i*2;if(ne)return 2*(4-o);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 63, column 3 - line 65, column 34): "+[u.constructor.name,o.constructor.name,i.constructor.name])}}};return n(2)(function(u){return e(Lt(d))(Tb(Tl)(hb))(2)(p(ff(C))(ke())(wb))(function(){var o=n(10)(function(i){return e(Lt(d))(Tb(Tl)(hb))(10)(p(ff(C))(ke())(wb))(It(Eb(C))(It(Eb(C))(p(ff(C))(a)(fh(t)))(u))(i))});return rh(Lt(d))(o)(Q(C)(oh)(o))}())})},g1=function(){return y.value}(),Ph=function(t){return function(r){return function(e){return function(n){return Ft({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(v))(v)(g1)({txt:B(oe(d)(`module Main

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
import FRP.Behavior (ABehavior, Behavior, behavior, sample, sampleBy, sample_, step, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- \`swell\` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = \u237A - \u03B2s
--   | mouse up   = \u0263 - \u03B4s - \u03B5 ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using \`solve2'\`.
swell :: Mouse -> Behavior Number
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

  fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
  fixB a fn = behavior \\s ->
    fix \\event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOn event s }

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
    -> ABehavior event t
    -> ABehavior event a
    -> ABehavior event a
  integral g initial t b =
    behavior \\e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\\) t x)
        z = fold approx y initial
      in
        sampleOn z e
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
    -> ABehavior event t
    -> ABehavior event t
    -> ABehavior event t
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
  )`)),empl:B(rt(tu()(d)(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(y.value)(function(a){return function(u){var o=N(L(l))(_(E(d))(void 0))(u.start);return Wr(v)([kn(v)(_n(Nt)(S(l))(p(C)(function(){var i=Y(ve)(se.value);return function(m){return i(Zr(x(m)))}}()))([Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(i){return i.value0})(n)))(Q(C)(o)(Z(tt))))(function(i){return function(){i();var s=oa(fe)(),c=Hu(fe)(s)(),D=uh(),b=c_(0)(1e4)(),f=function(ct){return{o:ct.value0+.04,n:ct.value1,t:qo}},h=p(io)(function(ct){return ct-.5})(k_(Sg)),et=W(Pf)(h)(function(ct){return W(Pf)(h)(function(_r){return W(Pf)(h)(function(hr){return W(Pf)(h)(function(nr){return _(y_)($r(Or(ua)()(mn)()(qa))(ct)($r(Or(ku)()(vn)()(mn))(_r)($r(Or(gu)()(Dn)()(vn))(hr)($r(Or(Cu)()(Eu)()(Dn))(nr)(Uu)))))})})})}),mt=It(If)(p(io)(ut.create)(et))(et),ur=It(If)(It(If)(It(If)(p(io)(function(ct){return function(_r){return function(hr){return function(nr){return{s0:ct,s1:_r,s2:hr,s3:nr}}}}})(mt))(mt))(mt))(mt),jt=wc(ur)({newSeed:Fc(b),size:5}),jr=el(s)(Qi(d)(p(C)(function(ct){return new ut(ct.acTime,ct.value)})(cl(s)(fl(C)(k1(D))(of))))(function(ct){return[We(it)(0)(p(C)(function(){var _r=Cn()(Ne),hr=Vn(dn)(function(nr){return Ou(Pa)(-.4)(.5*(nr-1))});return function(nr){return _r(f(hr(nr)))}}())(ct))([jc(zD(gt(kt()(J(J(At)(VC)()()()({reflectSymbol:function(){return"q"}}))(MD)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4,q:20})([FE(Lc)(90.4)])]),We(it)(0)(p(C)(function(){var _r=Cn()(Ne),hr=Vn(dn)(function(nr){return Ou(Pa)(-.2)(.4*(nr-3))});return function(nr){return _r(f(hr(nr)))}}())(ct))([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*4,q:20})([Ai(yi(gt(kt()(J(J(At)(bi(Di(ua)))()()()({reflectSymbol:function(){return"spec"}}))(di)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*3.02,spec:jt.s0})(N(L(l))(pt())(p(C)(function(){var _r=Ao()(Ne),hr=Vn(dn)(function(nr){return 90.4*3.02+14*(nr-1)});return function(nr){return _r(f(hr(nr)))}}())(ct)))])]),We(it)(0)(p(C)(function(){var _r=Cn()(Ne),hr=Vn(dn)(function(nr){return Ou(Pa)(-.1)(.2*(nr-6))});return function(nr){return _r(f(hr(nr)))}}())(ct))([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*6,q:20})([Ai(yi(gt(kt()(J(J(At)(bi(Di(ua)))()()()({reflectSymbol:function(){return"spec"}}))(di)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*5.07,spec:jt.s1})(N(L(l))(pt())(p(C)(function(){var _r=Ao()(Ne),hr=Vn(dn)(function(nr){return 90.4*5.07+18*(nr-1)});return function(nr){return _r(f(hr(nr)))}}())(ct)))])]),We(it)(0)(p(C)(function(){var _r=Cn()(Ne),hr=Vn(dn)(function(nr){return Ou(Pa)(0)(.2*(nr-3))});return function(nr){return _r(f(hr(nr)))}}())(ct))([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*8,q:20})([Ai(yi(gt(kt()(J(J(At)(bi(Di(ua)))()()()({reflectSymbol:function(){return"spec"}}))(di)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*7.13,spec:jt.s2})(N(L(l))(pt())(p(C)(function(){var _r=Ao()(Ne),hr=Vn(dn)(function(nr){return 90.4*7.13+32*(nr-1)});return function(nr){return _r(f(hr(nr)))}}())(ct)))])]),We(it)(0)(p(C)(function(){var _r=Cn()(Ne),hr=Vn(dn)(function(nr){return Ou(Pa)(0)(.1*(nr-7))});return function(nr){return _r(f(hr(nr)))}}())(ct))([Ai(yi(gt(kt()(J(J(At)(bi(Di(ua)))()()()({reflectSymbol:function(){return"spec"}}))(di)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:90.4*9.14,spec:jt.s3})(N(L(l))(pt())(p(C)(function(){var _r=Ao()(Ne),hr=Vn(dn)(function(nr){return 90.4*9.14+31*(nr-1)});return function(nr){return _r(f(hr(nr)))}}())(ct)))])]}))(),Qr=X(at)(X(at)(jr)(c))(gn(fe)(s));return t(X(at)(Qr)(a.start(void 0)))(),a.stop(Qr)()}}),Yr(C)(u.stop)(function(i){return X(at)(i)(X(at)(t(_(l)(void 0)))(a.start(void 0)))})]))([nn(d)(Ce(Nt)(S(l))([Q(C)(o)("Turn on"),Q(C)(u.stop)("Turn off")]))])])}})))})}}}};var E1=function(){return y.value}(),Ih=function(t){return function(r){return function(e){return function(n){var a=$a(t)(e);return Ft({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(U()(U()(an()(K(l))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}})(v))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})(v))(v)(E1)({next:Oa(L(l))(E(d))(n)(X(at)(r(J_.value))(fn)),fold:B(nh(a)(r)(e)(n)),fix:B(Ph(a)(r)(e)(n))})}}}};var T1=function(){function t(){}return t.value=new t,t}(),Rh=function(){function t(){}return t.value=new t,t}(),Pb=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),S1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import FRP.Event (bus)

import Math (pow)
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
main = runInBody1 (bus \\push -> lcmap (pure Init <|> _) \\event ->
  D.div_
    [ D.button
        ( event <#>
            \\e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_
                      [ gain_ 1.0
                          -- we create 100 cells
                          $ join
                          $ cell <$> 0 .. 100
                      ]
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ])
`;var x1=function(){return y.value}(),F1=function(t){return function(r){return function(e){return _(t)(jf(r)(Lf)({x:OD,o:e}))}}},O1=function(t){return function(r){return function(e){return _(t)(jf(r)(Lf)({x:PC,o:e}))}}},$1=Ya(Un)(zr)(function(t){var r=function(a){return N(L(l))(F1(E(d))()(a+.27*(t*tf(1.005)(t))))(O1(E(d))()(a+3+.3*(t*tf(1.005)(t))))},e=function(a){return _(E(d))(Cn()($n)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*tf(1.005)(t)),d:.8}))},n=function(a){return function(u){return We(it)(0)(e(a))([Hf(af)(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),Nh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(v))(y.value)(x1)({txt:B(oe(d)(S1)),ex0:B(rt(Be(d)(function(n){return Ya(Un)(function(a){return N(L(l))(_(E(d))(T1.value))(a)})(function(a){return Wr(v)([kn(v)(Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(u){return u.value0})(e)))(p(C)(ut.create)(a)))(function(u){return Y(ve)(se.value)(Zr(x(function(){return u.value0 instanceof Pb?X(at)(X(at)(u.value0.value0)(n(Rh.value)))(t(_(l)(void 0))):function(){u.value1();var i=Ks([Mt(it)(1)(Va(ti)(p(Fr)($1)(ln(0)(100))))])();return t(X(at)(i)(n(Rh.value)))(),n(new Pb(i))()}}())))}))([nn(d)(Yr(C)(a)(function(u){return u instanceof Pb?"Turn off":"Turn on"}))])])})})))})}}};var Si=function(){function t(){}return t.value=new t,t}();var lf={attr:function(t){return function(r){return A({key:"max",value:H(r)})}}};var xi=function(){function t(){}return t.value=new t,t}();var _f={attr:function(t){return function(r){return A({key:"min",value:H(r)})}}};var Fi=function(){function t(){}return t.value=new t,t}();var pf={attr:function(t){return function(r){return A({key:"input",value:lt(r)})}}};var Oi=function(){function t(){}return t.value=new t,t}(),sf={attr:function(t){return function(r){return A({key:"step",value:H(r)})}}};var $i=function(){function t(){}return t.value=new t,t}();var mf={attr:function(t){return function(r){return A({key:"value",value:H(r)})}}};var Xo=function(t){return function(r){return function(e){return N(t)(r)(e(void 0))}}};var w1=ig,lu={convert:function(t){return t}},ep={convert:function(t){return p_(t)}},Bh=function(t){return t},Ib=function(t){return t.convert},Ga=function(t){return function(r){return function(e){return dt(w1)(p_(r))(Ib(t)(e(void 0)))}}};var np=function(t){return function(r){return function(e){return function(n){return _n(fg)(r)(e)(Bh(Ib(t)(n)))}}}};function Wh(t){return t.target}var sl=function(t){return Ke(Wh(t))};var R1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
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
`,N1=function(){return y.value}(),L1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",qh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(U()(K(l))({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}})(v))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(v))(y.value)(N1)({wagtxt:B(oe(d)(`run2_
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
          (add <$> (pure 0.0 <|> sl1))`)),txt:B(oe(d)(R1)),ex1:B(rt(tu()(d)(Ru({reflectSymbol:function(){return"slider"}})()()()(ue({reflectSymbol:function(){return"s0"}})()()(ue({reflectSymbol:function(){return"s1"}})()()(ue({reflectSymbol:function(){return"s2"}})()()(Wn)()()()())()()()())()()()())(Ru({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"loading"}})()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())(y.value)(function(n){return function(a){var u=N(L(l))(a.startStop.start)(_(E(d))(void 0)),o=function(i){return lr(Wf(gt(kt()(J(J(J(J(At)(zc)()()()({reflectSymbol:function(){return"playbackRate"}}))(w_)()()()({reflectSymbol:function(){return"loopStart"}}))(M_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Uf)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Xo(L(l))(pt())(function(){return Xo(L(l))(p(C)(function(){var m=ia()($s),s=Ma(0)(.2)(100)(5);return function(c){return m(s(c))}}())(a.slider.s0))(function(){return Xo(L(l))(p(C)(function(){var m=T0(),s=Ma(0)(0)(100)(1.2);return function(c){return m(s(c))}}())(a.slider.s1))(function(){return p(C)(function(){var m=S0(),s=Ma(0)(.05)(100)(1);return function(c){return m(s(c))}}())(Fn(Lt(d))(a.slider.s2)(p(C)(Lr(Aa))(N(L(l))(_(E(d))(0))(a.slider.s1))))})})}))};return Wr(v)(dt(hn)(p(Fr)(function(i){return Wr(v)([oe(d)(i.l),go(v)(np(lu)(S(l))(_(E(d)))(Ga(lu)(Y(jo)(ko.value)("range"))(function(){return Ga(lu)(Y(_f)(xi.value)("0"))(function(){return Ga(lu)(Y(lf)(Si.value)("100"))(function(){return Ga(lu)(Y(sf)(Oi.value)("1"))(function(){return Ga(ep)(Y(mf)($i.value)("50"))(function(){return Y(pf)(Fi.value)(Zr(function(){var m=ae(l)(Jr)(tc(Tn)(Jf)(i.f)),s=Kn(Ca)(ol);return function(c){return m(s(sl(c)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([kn(v)(np(lu)(S(l))(p(C)(function(){var i=Y(ve)(se.value);return function(m){return i(Zr(x(m)))}}()))(Ga(lu)(Q(C)(a.startStop.loading)(_(l)(void 0)))(function(){return Ga(ep)(Yr(C)(a.startStop.stop)(function(i){return X(at)(i)(X(at)(t(_(l)(void 0)))(n.startStop.start(void 0)))}))(function(){return Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(i){return i.value0})(e)))(Q(C)(u)(Z(tt))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=yo(W(Re)(oa(Ie))(function(c){return W(Re)(Hu(Ie)(c))(function(D){return W(Re)(Ct(c)(L1))(function(b){return De(Ie)(function(){var h=vt(c)([o(b)])(),et=X(at)(X(at)(h)(D))(gn(fe)(c));return n.startStop.stop(et)(),et})})})}))();return t(function(){return n.startStop.start(void 0)(),Go(Ci(s))()})(),void 0}})})})))([nn(d)(Xo(L(l))(p(C)(x("Turn off"))(a.startStop.stop))(function(){return p(C)(x("Turn on"))(u)}))])]))}})))})}}};var U1=`module Main where

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
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (interval)
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

random :: Behavior Number
random = behavior \\e ->
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

        music :: forall lock. _ -> Event (Array (Audible _ lock _))
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
  )`,W1=cu(function(t){return qt(function(r){return xt(t)(function(e){return function(){var a=Bu();return r(e(a))()}})})}),q1=function(){return y.value}(),H1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(ne)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 226, column 1 - line 226, column 23): "+[t.constructor.name])},Hh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Ocarina comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Ocarina <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in ocarina is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The ocarina interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The ocarina interval needs an audio context to work.</li>
    <li>The ocarina interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(v))(y.value)(q1)({txt:B(oe(d)(U1)),ex2:B(rt(tu()(d)(ue({reflectSymbol:function(){return"slider"}})()()(Ru({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(Wn)()()()())()()()())(y.value)(function(n){return function(a){var u=N(L(l))(a.startStop.start)(_(E(d))(void 0)),o=function(i){return Qi(d)(i)(function(m){var s=p(C)(function(){var et=Lr(Aa)(.01);return function(mt){return et(tn(mt))}}())(m),c=p(C)(Ba)(m),D=N(L(l))(pt())(p(C)(function(){var et=Ao()($s);return function(mt){return et(H1(mt))}}())(c)),b=p(C)(function(et){return Fs(function(mt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:mt}}(et))})(s),f=p(C)(function(et){return Fs(function(mt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:mt}}(et))})(s),h=p(C)(function(et){return Fs(function(mt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:mt}}(et))})(s);return[xa(qs(Ms)(0)(D))(function(et){return function(mt){return Mt(it)(2)([We(it)(0)(p(C)(Cn()($n))(h))([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:1e3,q:20})([et])]),We(it)(0)(p(C)(Cn()($n))(f))([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2e3,q:20})([et])]),We(it)(0)(p(C)(Cn()($n))(b))([Jc(GD(gt(kt()(J(J(At)(XC)()()()({reflectSymbol:function(){return"q"}}))(wD)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:4e3,q:20})([et])])])}})]})};return Wr(v)([Wr(v)([oe(d)("tempo"),go(v)(np(lu)(S(l))(_(E(d)))(Ga(lu)(Y(jo)(ko.value)("range"))(function(){return Ga(lu)(Y(_f)(xi.value)("0"))(function(){return Ga(lu)(Y(lf)(Si.value)("100"))(function(){return Ga(lu)(Y(sf)(Oi.value)("1"))(function(){return Ga(ep)(Y(mf)($i.value)("50"))(function(){return Y(pf)(Fi.value)(Zr(function(){var i=ae(l)(Jr)(tc(Tn)(Jf)(n.slider)),m=Kn(Ca)(ol);return function(s){return i(m(sl(s)))}}()))})})})})})))([])]),kn(v)(_n(Nt)(S(l))(p(C)(function(){var i=Y(ve)(se.value);return function(m){return i(Zr(x(m)))}}()))([Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(i){return i.value0})(e)))(Q(C)(u)(Z(tt))))(function(i){return function(){i();var s=oa(fe)(),c=Ti(C)(ut.create)(W1)(eh(s)(.91)(p(C)(Ma(0)(.42)(100)(1.4))(a.slider))),D=el(s)(o(c))(),b=X(at)(D)(gn(fe)(s));return t(X(at)(b)(n.startStop.start(void 0)))(),n.startStop.stop(X(at)(b)(gn(fe)(s)))()}}),Yr(C)(a.startStop.stop)(function(i){return X(at)(i)(X(at)(t(_(l)(void 0)))(n.startStop.start(void 0)))})]))([nn(d)(Ce(Nt)(S(l))([Q(C)(u)("Turn on"),Q(C)(a.startStop.stop)("Turn off")]))])])}})))})}}};var G1=function(){return y.value}(),zh=function(){return sr({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(v)(K(l))(y.value)(G1)({})}();var J1=function(){return y.value}(),Gh=function(){return sr({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(v)(K(l))(y.value)(J1)({})}();var X1=function(){return y.value}(),Vh=function(){return sr({reflectType:function(){return`<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a =
  ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(v)(K(l))(y.value)(X1)({})}();var K1=function(){return y.value}(),Jh=function(t){return function(r){return function(e){return function(n){var a=function(o){return Oa(L(l))(E(d))(n)(X(at)(r(o))(fn))},u=$a(t)(e);return sr({reflectType:function(){return`<div>
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
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(U()(U()(U()(an()(U()(K(l))({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}})(v))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}})(v))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}})(v))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(v))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(v))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(v))(y.value)(K1)({next:a(G_.value),primer:B(Vh),inOcarina:B(Gh),flavors:B(zh),ex0:B(Nh(u)(r)(n)),ex1:B(qh(u)(r)(n)),ex2:B(Hh(u)(r)(n))})}}}};var Z1=function(){return y.value}(),jh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(v))(y.value)(Z1)({ai0:B(rt(ht(e)(t)(function(n){return bo(wn)(It(gi)(It(gi)(It(gi)(p(Zc)(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})(Mn(wn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Mn(wn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Mn(wn)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Mn(wn)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return vt(n)([Mt(it)(1)(function(){var u=function(o){return _(E(d))(jf()(Lf)(Os()(Lr(Aa)(o))(F_)))};return[jn(Ha)(a.tink0)(u(.1)),jn(Ha)(a.tink1)(u(.2)),jn(Ha)(a.tink2)(u(.9)),jn(Ha)(a.tink3)(u(1.8))]}())])}})))})}}};var rL=function(){return y.value}(),Xh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(v))(y.value)(rL)({ai0:B(rt(ht(e)(t)(function(n){return bo(wn)(It(gi)(It(gi)(It(gi)(p(Zc)(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})(Mn(wn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Mn(wn)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Mn(wn)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Mn(wn)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return vt(n)([Mt(it)(1)(function(){var u=function(i){return _(E(d))(jf()(Lf)(Os()(Lr(Aa)(i))(F_)))},o=function(i){var m=ja(Qu)(i)(4);return m===0?a.tink0:m===1?a.tink1:m===2?a.tink2:a.tink3};return Yr(Fr)(ln(0)(100))(function(i){var m=zr(i);return jn(Ha)(o(i))(u(.3+.3*(m*tf(1.005)(m))))})}())])}})))})}}};var nL=function(){return y.value}(),Qh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(v))(y.value)(nL)({ai0:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([xa(lr(gr)(a)(pt()))(function(u){return function(o){return Mt(it)(.8)([An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:400,q:1})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:880,q:5})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:1200,q:10})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:2e3,q:20})([u]),An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var uL=function(){return y.value}(),Kh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(v))(y.value)(uL)({ai0:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([xa(lr(gr)(a)(pt()))(function(u){return function(o){return Mt(it)(.8)(Yr(Fr)(ln(0)(40))(Ya(Un)(zr)(function(i){return An(on(gt(kt()(J(J(At)(yn)()()()({reflectSymbol:function(){return"q"}}))(un)()()()({reflectSymbol:function(){return"frequency"}})))(yt()())))({frequency:200+i*150,q:30})([u])})))}})])}})))})}}};var iL=function(){return y.value}(),Yh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(v))(y.value)(iL)({ai0:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return vt(n)([nu(function(u){return Mt(it)(1)([jn(Ha)(a)(pt()),Do(Ye)(.1)([Mt(it)(.6)([u])])])})])}})))})}}};var cL=function(){return y.value}(),lL=function(t){return function(r){return _(t)(Cn(r)($n)({p:[1,1,0],o:0,d:10}))}},_L=function(t){return function(r){return _(t)(Cn(r)($n)({p:[1,1,0],o:0,d:8}))}},ml=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return Do(t)(n)([Mt(r)(a)([Jc(e)(u)(o)])])}}}}}}},Zh=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(v))(y.value)(cL)({txt:B(oe(d)(`dgh d g h i =
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
  ]`)),ai0:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return vt(n)([xa(jn(Ha)(a)(pt()))(function(u){return function(o){return nu(function(i){return Mt(it)(1)([u,ml(Ye)(it)(eu)(.15)(.7)(1500)([nu(function(m){return We(it)(1)(lL(E(d))())([ml(Ye)(it)(eu)(.4)(.5)(2500)([i,m])])})]),ml(Ye)(it)(eu)(.29)(.85)(2e3)([nu(function(m){return Mt(it)(1)([ml(Ye)(it)(eu)(.6)(.6)(3500)([i,nu(function(s){return We(it)(1)(_L(E(d))())([ml(Ye)(it)(eu)(.75)(.6)(4e3)([m,s]),ml(Ye)(it)(eu)(.75)(.55)(3e3)([u])])})])])})])])})}})])}})))})}}};var sL=function(){return y.value}(),tT=function(t){return function(r){return function(e){return function(n){var a=function(u){return Oa(L(l))(E(d))(n)(X(at)(r(u))(fn))};return sr({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(v)(an()(K(l))({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})(v))(y.value)(sL)({hwLink:a(zf.value)})}}}};var vL=function(){return y.value}(),rT=function(t){return function(r){return function(e){return function(n){var a=function(o){return Oa(L(l))(E(d))(n)(X(at)(r(o))(fn))},u=$a(t)(e);return sr({reflectType:function(){return`<div>
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

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself \u{1F92F}. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Ocarina doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(U()(U()(U()(U()(U()(an()(K(l))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}})(v))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}})(v))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}})(v))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}})(v))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}})(v))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}})(v))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})(v))(y.value)(vL)({intro:B(tT(t)(r)(e)(n)),next:a(H_.value),code0:B(jh(u)(r)(n)),code1:B(Xh(u)(r)(n)),code2:B(Qh(u)(r)(n)),code3:B(Kh(u)(r)(n)),code4:B(Yh(u)(r)(n)),code5:B(Zh(u)(r)(n))})}}}};var eT=function(t){return function(r){return function(e){return new P(j(t)("code")(r)(q(e)))}}},Lb=function(t){return eT(t)(M(S(t.MonadST5().Monad0().Applicative0())))};var nT=function(t){return function(r){return function(e){return new P(j(t)("pre")(r)(q(e)))}}},Bb=function(t){return nT(t)(M(S(t.MonadST5().Monad0().Applicative0())))};var yL=function(){return y.value}(),aT=function(t){return function(r){return function(e){return function(n){var a=X(at)(r(z_.value))(fn),u=$a(t)(e);return sr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(v)(U()(an()(U()(K(l))({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}})(v))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(v))(y.value)(yL)({code:B(Bb(v)([Lb(v)([oe(d)(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:B(rt(ht(n)(u)(function(o){return _(da)(void 0)})(function(o){return function(i){return vt(o)([Mt(it)(.15)([Hf(af)(440)(pt())])])}}))),next:Oa(L(l))(E(d))(n)(a)})}}}};var uT=Cc;var oT=function(){return function(t){return t}},iT=function(){return function(t){return t}};var Ub=function(){function t(){}return t.value=new t,t}();var fT={attr:function(t){return function(r){return A({key:"height",value:H(r)})}}};var Wb=function(){function t(){}return t.value=new t,t}();var cT={attr:function(t){return function(r){return A({key:"width",value:H(r)})}}};var qb=function(t){return function(r){return function(e){return new P(j(t)("canvas")(r)(q(e)))}}};var Hb=function(){function t(){}return t.value=new t,t}(),zb={attr:function(t){return function(r){return A({key:"@self@",value:lt(r)})}}};function mm(t){return function(){return t.getContext("2d")}}function ap(t){return function(r){return function(){t.fillStyle=r}}}function vm(t){return function(){t.beginPath()}}function Dm(t){return function(){t.fill()}}function Gb(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function dm(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var NL=function(){return 2*rf}(),vl=function(t){return{o:t.value0+.04,n:t.value1,t:qo}};var LL=function(){return y.value}(),Dl=function(t){return function(r){return function(e){return function(n){return _(t)(Ao(r)($n)({p:[e,n],o:0,d:16}))}}}},BL=function(t){return function(r){return _(t)(Cn(r)($n)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},UL=function(t){return function(r){return _(t)(Cn(r)($n)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var bm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(m){return function(s){return B_(t)(n)(a)([We(r)(u)(o)([YD(e)(i)(m)(s)])])}}}}}}}}}},lT=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(m){return function(s){return B_(t)(n)(a)([We(r)(u)(o)([KD(e)(i)(m)(s)])])}}}}}}}}}},WL=function(t){return function(r){return function(e){return function(n){return _(t)(il(r)($n)({p:[e,n],o:0,d:16}))}}}},_T=400,Vb=zr(_T),qL=function(){return Gt(Ja)(_T)+"px"}(),pT=600,Jb=zr(pT),HL=function(){return Gt(Ja)(pT)+"px"}(),zL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},sT=function(t){return function(r){return function(e){return sr({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(v))(y.value)(LL)({ex1:B(rt(tu()(d)(ue({reflectSymbol:function(){return"canvas"}})()()(ue({reflectSymbol:function(){return"slider"}})()()(Ru({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"loading"}})()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())()()()())(y.value)(function(n){return function(a){var u=N(L(l))(_(E(d))(void 0))(a.startStop.start),o=function(i){return function(m){return function(s){var c=p(C)(function(D){return new ut(D.acTime,D.value)})(cl(i)(a.slider));return[Ws(Us(gt(kt()(J(J(At)(Bs)()()()({reflectSymbol:function(){return"fftSize"}}))(Ls)()()()({reflectSymbol:function(){return"cb"}})))(yt()())))({cb:function(D){return function(){return Sn(new R(D))(s)(),Sn(z.value)(s)}},fftSize:Ss.value})(_(_e)(xa(jn(Ha)(m)(N(L(l))(pt())(p(C)(function(){var D=ia()(Ne),b=Vn(dn)(Ma(0)(.96)(100)(1.04));return function(f){return D(vl(b(f)))}}())(c))))(function(D){return function(b){return nu(function(f){return Mt(it)(1)([D,B_(VD(gt(kt()(J(J(At)(QC)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(PD)()()()({reflectSymbol:function(){return"delayTime"}})))(yt()())))({maxDelayTime:2.5,delayTime:1})(p(C)(function(){var h=il()(Ne),et=Vn(dn)(Ma(0)(.5)(100)(2.45));return function(mt){return h(vl(et(mt)))}}())(c))([We(it)(.4)(p(C)(function(){var h=Cn()(Ne),et=Vn(dn)(Ma(0)(.6)(100)(.9));return function(mt){return h(vl(et(mt)))}}())(c))([D])]),bm(Ye)(it)(eu)(.15)(M(S(l)))(.7)(M(S(l)))(1500)(Dl(E(d))()(1500)(3e3))([nu(function(h){return We(it)(1)(BL(E(d))())([bm(Ye)(it)(eu)(.4)(M(S(l)))(.5)(M(S(l)))(3e3)(Dl(E(d))()(3e3)(100))([f,h])])})]),bm(Ye)(it)(eu)(.29)(p(C)(function(){var h=il()(Ne),et=Vn(dn)(Ma(0)(.1)(100)(.4));return function(mt){return h(vl(et(mt)))}}())(c))(.85)(M(S(l)))(2e3)(Dl(E(d))()(2e3)(5e3))([nu(function(h){return Mt(it)(1)([bm(Ye)(it)(eu)(.6)(p(C)(function(){var et=il()(Ne),mt=Vn(dn)(Ma(0)(.8)(100)(.3));return function(ur){return et(vl(mt(ur)))}}())(c))(.6)(M(S(l)))(3500)(Dl(E(d))()(3500)(100))([f,nu(function(et){return We(it)(1)(UL(E(d))())([lT(Ye)(it)(JD)(.75)(p(C)(function(){var mt=il()(Ne),ur=Vn(dn)(Ma(0)(.9)(100)(.1));return function(jt){return mt(vl(ur(jt)))}}())(c))(.6)(M(S(l)))(4e3)(Dl(E(d))()(4e3)(200))([h,et]),lT(Ye)(it)(JD)(.75)(WL(E(d))()(.75)(.2))(.55)(M(S(l)))(200)(Dl(E(d))()(200)(4e3))([D])])})])])})])])})}})))]}}};return Wr(v)([qb(v)(N(L(l))(_n(Nt)(S(l))(_(E(d)))([Y(cT)(Wb.value)(HL),Y(fT)(Ub.value)(qL),Y(wk)(Vt.value)("width: 100%;"),Y(zb)(Hb.value)(function(){var i=ae(l)(Jr)(function(m){return function(){var c=mm(m)();return ap(c)("black")(),dm(c)({width:Jb,height:Vb,x:0,y:0})(),void 0}});return function(m){return i(Db(m))}}())]))(p(C)(function(i){return Y(zb)(Hb.value)(function(){var m=ae(l)(Jr)(function(s){return function(){var D=mm(s)();return ap(D)("black")(),dm(D)({width:Jb,height:Vb,x:0,y:0})(),ap(D)("rgba(255,255,255,0.2)")(),Sl(i)(function(b){return function(){return vm(D)(),Gb(D)({end:NL,radius:b.value1*40,start:0,x:b.value0.x*Jb,y:b.value0.y*Vb,useCounterClockwise:!1})(),Dm(D)()}})()}});return function(s){return m(Db(s))}}())})(a.canvas)))([]),go(v)(_n(Nt)(S(l))(_(E(d)))([Y(jo)(ko.value)("range"),Y(_f)(xi.value)("0"),Y(lf)(Si.value)("100"),Y(sf)(Oi.value)("1"),Y(mf)($i.value)("50"),Y(Mk)(Vt.value)("width: 100%;"),Y(pf)(Fi.value)(Zr(function(){var i=ae(l)(Jr)(tc(Tn)(Jf)(n.slider)),m=Kn(Ca)(ol);return function(s){return i(m(sl(s)))}}()))]))([]),kn(v)(Ce(Nt)(S(l))([_(E(d))(Y(Ec)(Vt.value)("width:100%; padding:1.0rem;")),_n(Nt)(S(l))(p(C)(function(){var i=Y(ve)(se.value);return function(m){return i(Zr(x(m)))}}()))([Q(C)(a.startStop.loading)(_(l)(void 0)),Yr(C)(a.startStop.stop)(function(i){return X(at)(i)(X(at)(t(_(l)(void 0)))(n.startStop.start(void 0)))}),Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(i){return i.value0})(e)))(Q(C)(u)(Z(tt))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=pe(z.value)(),c=yo(W(Re)(oa(Ie))(function(D){return W(Re)(Hu(Ie)(D))(function(b){return W(Re)(p(ki)(iT())(GE(wn)(uT)(Ct(D))(oT()(zL))))(function(f){return W(Re)(De(Ie)(c_(0)(5e4)))(function(h){var et=wc(nD(ga(p_(f.pluck0))(Oc(hv(Tv()(f))))))({newSeed:Fc(h),size:4});return De(Ie)(function(){var ur=Bn(Fo)(l)(function(ct){return function(){var hr=Bu(),nr=Bu();return{x:hr,y:nr}}})(ln(0)(127))(),jt=vt(D)(o(D)(et)(s))(),jr=xt(of)(function(ct){return function(){var hr=Pe(s)();return Ln(l)(Jr)(hr)(function(nr){return function(){var Co=j_(nr)(),zu=p(F)(function(){var Mi=Bl(ur),_u=p(Fr)(function(cn){return function(Qo){return Qo/255}(cn)});return function(cn){return Mi(_u(cn))}}())(Zs(Ys)(Co))();return n.canvas(zu)(),void 0}})()}})(),Qr=X(at)(X(at)(X(at)(jt)(b))(gn(fe)(D)))(jr);return n.startStop.stop(Qr)(),Qr})})})})}))();return t(function(){return n.startStop.start(void 0)(),Go(Ci(c))()})(),void 0}})])]))([nn(d)(Ce(Nt)(S(l))([p(C)(x("Turn off"))(a.startStop.stop),p(C)(x("Turn on"))(u),p(C)(x("Loading..."))(a.startStop.loading)]))])])}})))})}}};var VL=function(){return y.value}(),mT=function(t){return function(r){return function(e){return function(n){var a=$a(t)(e);return Ft({reflectType:function(){return`<div>
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

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la ocarina</a>!</p>
</div>`}})()()(U()(an()(K(l))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})(v))(v)(VL)({next:Oa(L(l))(E(d))(n)(X(at)(r(zf.value))(fn)),ex:B(sT(a)(r)(n))})}}}};var jL=function(){return y.value}(),vT=function(t){return function(r){return function(e){return function(n){return Ft({reflectType:function(){return`<div>
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
</div>`}})()()(an()(K(l))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))(v)(jL)({next:_(E(d))(Y(ve)(se.value)(Zr(x(X(at)(r(zs.value))(fn)))))})}}}};var QL=function(){return y.value}(),DT=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(v))(v)(QL)({txt:B(oe(d)(`\\ctx buf -> run2 ctx
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
  ]`)),cancel:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Mt(it)(1)([lr(gr)(a)(Ce(Nt)(S(l))([pt(),Iu(1e3)(_(E(d))(ia()($n)({p:Va(ti)(Q(Fr)(ln(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Iu(3e3)(_(E(d))(ia()(RC)({o:3.5})))]))])])}})))})}}};var YL=function(){return y.value}(),dT=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(v))(v)(YL)({txt:B(oe(d)(`\\ctx buf -> run2 ctx
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
  ]`)),envelope:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Mt(it)(1)([lr(gr)(a)(Ce(Nt)(S(l))([pt(),Iu(1e3)(_(E(d))(ia()($n)({p:Va(ti)(Q(Fr)(ln(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})))})}}};var tB=function(){return y.value}(),bT=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})(v))(y.value)(tB)({numericEx:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Mt(it)(1)([lr(gr)(a)(Xo(L(l))(pt())(function(){return Xo(L(l))(Iu(1e3)(Xo(L(l))(_(E(d))(ia()(Ne)({n:1,o:1,t:FD})))(function(){return _(E(d))(ia()(Ne)({n:1.3,o:2,t:qo}))})))(function(){return Iu(2500)(Xo(L(l))(_(E(d))(ia()(Ne)({n:1,o:2.5,t:FD})))(function(){return _(E(d))(ia()(Ne)({n:.7,o:3.5,t:IC}))}))})}))])])}})))})}}};var eB=function(){return y.value}(),yT=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(v)(U()(K(l))({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})(v))(y.value)(eB)({suddenEx:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([Mt(it)(1)([lr(gr)(a)(Ce(Nt)(S(l))([pt(),Iu(1500)(_(E(d))(ia()(wC)({n:1.4})))]))])])}})))})}}};var aB=function(){return y.value}(),AT=function(t){return function(r){return function(e){return Ft({reflectType:function(){return`<section>
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
`}})()()(U()(K(l))({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})(v))(v)(aB)({unitEx:B(rt(ht(e)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return vt(n)([lr(gr)(a)(Ce(Nt)(S(l))([pt(),_(E(d))(ia()(OC(vi)(vi))(FC(Mt(it)(1)([Hs(ws)(1)(pt()),Mt(it)(.2)([jc(Is)(100)([N_(Lc)(50)(pt())])])]))))]))])}})))})}}};var oB=function(){return y.value}(),kT=function(t){return function(r){return function(e){return function(n){var a=X(at)(r(V_.value))(fn),u=$a(t)(e);return Ft({reflectType:function(){return`<div>
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
</div>`}})()()(U()(U()(an()(U()(U()(U()(K(l))({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}})(v))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}})(v))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}})(v))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(v))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(v))(v)(oB)({sudden:B(yT(u)(r)(n)),numeric:B(bT(u)(r)(n)),envelope:B(dT(u)(r)(n)),cancel:B(DT(u)(r)(n)),unit:B(AT(u)(r)(n)),next:Oa(L(l))(E(d))(n)(a)})}}}};var fB=function(){return y.value}(),gT=function(t){return function(r){return function(e){return function(n){return Ft({reflectType:function(){return`<div>
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
</div>`}})()()(an()(K(l))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))(v)(fB)({next:_(E(d))(Y(ve)(se.value)(Zr(x(X(at)(r(Gs.value))(fn)))))})}}}};var lB=function(){return y.value}(),CT=function(t){return function(r){return function(e){return function(n){return Ft({reflectType:function(){return`<div>
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
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via ocarina. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by ocarina. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a ocarina graph</a>.</p>
</div>`}})()()(an()(K(l))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(v))(v)(lB)({next:_(E(d))(Y(ve)(se.value)(Zr(x(X(at)(r(Gf.value))(fn)))))})}}}};var pB=function(){return y.value}(),ET=function(t){return function(r){return function(e){return function(n){return sr({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(v)(K(l))(y.value)(pB)({})}}}};var mB=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
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

random :: Behavior Number
random = behavior \\e ->
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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
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
`,vB=cu(function(t){return qt(function(r){return xt(t)(function(e){return function(){var a=Bu();return r(e(a))()}})})}),DB=function(){return y.value}(),dB="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",hT=function(t){return function(r){return function(e){return sr({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(v)(U()(U()(K(l))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(v))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(v))(y.value)(DB)({txt:B(oe(d)(mB)),ex1:B(rt(tu()(d)(ue({reflectSymbol:function(){return"slider"}})()()(Ru({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"loading"}})()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())(y.value)(function(n){return function(a){var u=N(L(l))(_(E(d))(void 0))(a.startStop.start),o=Ti(C)(ut.create)(vB)(Pu(Lt(d))(function(m){return function(s){return s+1|0}})(a.slider)(0)),i=function(m){return[Mt(it)(1)([qp(p(C)(function(s){return Ce(Nt)(S(l))([_(E(d))(SC(jn(P_(gt(kt()(J(J(At)(HC)()()()({reflectSymbol:function(){return"playbackRate"}}))($_)()()()({reflectSymbol:function(){return"buffer"}})))(yt()())))({buffer:m,playbackRate:.7+Ba(s)*2})(pt()))),Iu(5e3)(_(E(d))(xC))])})(o))])]};return Wr(v)([Wr(v)([oe(d)("Slide me!"),go(v)(_n(Nt)(S(l))(_(E(d)))([Y(jo)(ko.value)("range"),Y(_f)(xi.value)("0"),Y(lf)(Si.value)("100"),Y(sf)(Oi.value)("1"),Y(mf)($i.value)("50"),Y(pf)(Fi.value)(Zr(x(n.slider(void 0))))]))([])]),kn(v)(_n(Nt)(S(l))(p(C)(function(){var m=Y(ve)(se.value);return function(s){return m(Zr(x(s)))}}()))([Q(C)(a.startStop.loading)(_(l)(void 0)),Yr(C)(a.startStop.stop)(function(m){return X(at)(m)(X(at)(t(_(l)(void 0)))(n.startStop.start(void 0)))}),Yr(C)(Fn(Lt(d))(N(L(l))(_(E(d))(_(l)(void 0)))(p(C)(function(m){return m.value0})(e)))(Q(C)(u)(Z(tt))))(function(m){return function(){m(),n.startStop.loading(void 0)();var c=yo(W(Re)(oa(Ie))(function(D){return W(Re)(Hu(Ie)(D))(function(b){return W(Re)(Ct(D)(dB))(function(f){return De(Ie)(function(){var et=Ks(i(f))(),mt=X(at)(X(at)(et)(b))(gn(fe)(D));return n.startStop.stop(mt)(),mt})})})}))();return t(function(){return n.startStop.start(void 0)(),Go(Ci(c))()})(),void 0}})]))([nn(d)(Ce(Nt)(S(l))([p(C)(x("Turn off"))(a.startStop.stop),p(C)(x("Turn on"))(u)]))])])}})))})}}};var yB=function(){return y.value}(),TT=function(t){return function(r){return function(e){return function(n){var a=$a(t)(e);return Ft({reflectType:function(){return`<div>
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
</div>`}})()()(U()(U()(K(l))({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}})(v))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})(v))(v)(yB)({appl:B(rt(Qs("\u{1F44F}")(n)(a)(function(u){return Ct(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(o){return vt(u)([Mt(it)(1)([lr(gr)(o)(pt())])])}}))),suby:B(hT(a)(r)(n))})}}}};var BLt=function(t){return t},ULt={Coercible0:function(){}},kB=function(t){return function(r){var e=function(a){var u=function(o){if(o instanceof q_)return Wr(v)(_(_e)(rt(Be(d)(mT(a.setCancellation)(a.setPage)))));if(o instanceof zf)return Wr(v)(_(_e)(rt(Be(d)(aT(a.setCancellation)(a.setPage)))));if(o instanceof z_)return Wr(v)(_(_e)(rt(Be(d)(rT(a.setCancellation)(a.setPage)))));if(o instanceof H_)return Wr(v)(_(_e)(rt(Be(d)(Z0(a.setCancellation)(a.setPage)))));if(o instanceof zs)return Wr(v)(_(_e)(rt(Be(d)(CT(a.setCancellation)(a.setPage)))));if(o instanceof Gf)return Wr(v)(_(_e)(rt(Be(d)(Jh(a.setCancellation)(a.setPage)))));if(o instanceof G_)return Wr(v)(_(_e)(rt(Be(d)(kT(a.setCancellation)(a.setPage)))));if(o instanceof V_)return Wr(v)(_(_e)(rt(Be(d)(Ih(a.setCancellation)(a.setPage)))));if(o instanceof Gs)return Wr(v)(_(_e)(rt(Be(d)(ET(a.setCancellation)(a.setPage)))));if(o instanceof a0)return Wr(v)(_(_e)(rt(Be(d)(vT(a.setCancellation)(a.setPage)))));if(o instanceof J_)return Wr(v)(_(_e)(rt(Be(d)(TT(a.setCancellation)(a.setPage)))));if(o instanceof u0)return Wr(v)(_(_e)(rt(Be(d)(gT(a.setCancellation)(a.setPage)))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 145, column 5 - line 145, column 80): "+[o.constructor.name])};return u(a.page)},n=Pu(Lt(d))(function(a){if(a instanceof tl)return function(u){return{prevPage:new R(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof ed)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 135, column 7 - line 137, column 75): "+[a.constructor.name])})(r)({prevPage:z.value,curPage:q_.value,cancel:_(l)(void 0),pageChange:!0});return[Wr(v)(p(Fr)(function(a){return Nv(v)([Rv(v)(N(L(l))(_n(Nt)(S(l))(_(E(d)))([Y(ve)(se.value)(Zr(x(t(new tl(a.value0))))),Y(Pk)(Vt.value)("cursor:pointer;")]))(p(C)(function(u){return Y(ve)(se.value)(Zr(x(function(){return u.cancel(),t(new tl(a.value0))()})))})(Vl(du(l))(function(){var u=Qa(La);return function(o){return u(function(i){return i.pageChange}(o))}}())(n))))([oe(d)(a.value1.value0)]),Tc(v)(_(E(d))(Y(Kp)(Vt.value)(function(){return a.value1.value1?"":"display:none;"}())))([oe(d)(" | ")])])})([new ut(q_.value,new ut("Home",!0)),new ut(zf.value,new ut("Hello world",!0)),new ut(z_.value,new ut("Array, fan, and fix",!0)),new ut(H_.value,new ut("Audio units",!0)),new ut(Gf.value,new ut("Events",!0)),new ut(G_.value,new ut("Parameters",!0)),new ut(V_.value,new ut("State",!0)),new ut(J_.value,new ut("Subgraphs",!1))])),Wr(v)(_(_e)(Fv(d)(function(a){return e({page:a.curPage,setPage:function(u){return t(tl.create(u))},setCancellation:function(u){return t(ed.create(u))}})})(Vl(du(l))(function(a){return a.pageChange})(n))))]}},WLt=function(t){return{page:t,setPage:$t(Xe(Ae(we))),setCancellation:$t(Xe(Ae(we)))}},qLt=function(){var r=W(Tn)(W(Tn)(hi)(bb))(y0)();return Ln(l)(Jr)(p(Me)(A0)(r))(function(e){return function(){var a=Lv(),u=pe(0)(),o=Yl(d)(d)(),i=Ck(v)(e)(kB(o.push)(o.event))(Fg(u));return yr(F)(xt(i)(function(m){return m(a)}))(),o.push(new tl(q_.value))()}})()};export{BLt as TopLevelSg,qLt as main,ULt as newtypeTopLevelSg_,WLt as p2tl,kB as scene};
