function zb(t){return()=>t.slice()}function Gb(t){return r=>e=>()=>{e[t]=r}}function Vb(t){return()=>t.slice()}var Jb=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var jb=Jb,Xb=Jb;var Qb=function(t){return function(r){return t===r}};var Kb=Qb,Yb=Qb;var d=function(){function t(){}return t.value=new t,t}();var Ae=function(t){return t.reflectSymbol};var ll=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Pa=function(t){return function(r){return r[t]}},Vu=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var _l={eq:Yb},Pi={eq:Kb};var Zt=function(t){return t.eq};var tr=function(){function t(){}return t.value=new t,t}(),br=function(){function t(){}return t.value=new t,t}(),yr=function(){function t(){}return t.value=new t,t}();var Zb=function(t){return function(r){return t-r|0}},ty=function(t){return function(r){return t-r}};var ry=function(t){return function(r){return t+r|0}},ey=function(t){return function(r){return t*r|0}},ny=function(t){return function(r){return t+r}},ay=function(t){return function(r){return t*r}};var ka=function(t){return t.zero};var ga={add:ny,zero:0,mul:ay,one:1},Fu={add:ry,zero:0,mul:ey,one:1};var Ca=function(t){return t.one};var In=function(t){return t.mul};var Br=function(t){return t.add};var Ou=function(t){return t.sub};var mf={sub:ty,Semiring0:function(){return ga}},_m={sub:Zb,Semiring0:function(){return Fu}};var pl=function(t){return function(r){return Ou(t)(ka(t.Semiring0()))(r)}};var Ia=function(){return{compare:Xb(tr.value)(yr.value)(br.value),Eq0:function(){return _l}}}(),Xe=function(){return{compare:jb(tr.value)(yr.value)(br.value),Eq0:function(){return Pi}}}();var rr=function(t){return t.compare};var oy=function(t){return function(r){return function(e){var n=rr(t)(r)(e);return!(n instanceof tr)}}};var $u=function(t){return function(r){return function(e){var n=rr(t)(r)(e);if(n instanceof tr)return e;if(n instanceof yr||n instanceof br)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var sm=function(t){return function(r){return function(e){var n=oy(t)(e)(ka(r.Semiring0()));return n?e:pl(r)(e)}}};var iy=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,i=0;i<e;i++)for(var o=t[i],m=0;m<n;m++)a[u++]=o(r[m]);return a}};var Yo={compose:function(t){return function(r){return function(e){return t(r(e))}}}},Mu=function(t){return t.compose};var tt=function(t){return t.identity},rt={identity:function(t){return t},Semigroupoid0:function(){return Yo}};var ne=!0;var xt=function(t){return function(r){return function(e){return t(e)(r)}}},x=function(t){return function(r){return t}};var Qf=function(t){return function(r){return r(t)}},ml=function(t){return function(r){return t(r)}};var fy=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var _=function(t){return t.map},Qr=function(t){return function(r){return function(e){return _(t)(e)(r)}}},Ar=function(t){return _(t)(x(void 0))},Q=function(t){return function(r){return function(e){return _(t)(x(e))(r)}}},Z_=function(t){return function(r){return _(t)(x(r))}};var fa={map:Mu(Yo)},Or={map:fy},mm=function(t){return function(r){return function(e){return _(t)(function(n){return n(e)})(r)}}};var vl={apply:iy,Functor0:function(){return Or}},qt=function(t){return t.apply};var X=function(t){return function(r){return function(e){return qt(t)(_(t.Functor0())(x(tt(rt)))(r))(e)}}},ca=function(t){return function(r){return function(e){return function(n){return qt(t)(_(t.Functor0())(r)(e))(n)}}}};var S=function(t){return t.pure};var Rn=function(t){return function(r){return function(e){if(r)return e;if(!r)return S(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,e.constructor.name])}}},Dl=function(t){return function(r){return function(e){return qt(t.Apply0())(S(t)(r))(e)}}};var pe={pure:function(t){return[t]},Apply0:function(){return vl}};var cy=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var ae=function(t){return t.discard};var Zo={bind:cy,Apply0:function(){return vl}},H=function(t){return t.bind},Qn=function(t){return xt(H(t))};var Kf=function(t){return function(r){return function(e){return function(n){return H(t)(r(n))(e)}}}};var jr={discard:function(t){return H(t)}};var Ja=function(t){return function(r){return H(t)(r)(tt(rt))}};var lt=function(t){return t};var ly=lt;var _y=function(t){return function(r){return function(){return t(r())}}};function la(t){return function(){return{value:t}}}var Nn=function(t){return function(){return t.value}},py=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},ja=function(t){return function(r){return function(){return r.value=t}}};var ju=function(t){return function(r){return function(e){return H(t.Bind1())(r)(function(n){return H(t.Bind1())(e)(function(a){return S(t.Applicative0())(n(a))})})}}};var sy=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var Df=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var my={append:function(t){return function(r){return void 0}}};var hn={append:sy};var bt=function(t){return t.append},dm=function(t){return{append:function(r){return function(e){return function(n){return bt(t)(r(n))(e(n))}}}}};var N=function(t){return t.alt};var UT=String.fromCharCode(65535),WT=String.fromCharCode(0),qT=Number.POSITIVE_INFINITY,HT=Number.NEGATIVE_INFINITY;var Kn=function(t){return t.top};var df={top:2147483647,bottom:-2147483648,Ord0:function(){return Xe}};var Yn=function(t){return t.bottom};var Dy=function(t){return t.toString()},dy=function(t){var r=t.toString();return isNaN(r+".0")?r:r+".0"};var rp={show:dy},Xa={show:Dy};var zt=function(t){return t.show};var V=function(){function t(){}return t.value=new t,t}(),U=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Yt=function(t){return function(r){return function(e){if(e instanceof V)return t;if(e instanceof U)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Pe={map:function(t){return function(r){return r instanceof U?new U(t(r.value0)):V.value}}};var Ea=function(t){return Yt(t)(tt(rt))},ta=function(){return function(t){if(t instanceof U)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ri={apply:function(t){return function(r){if(t instanceof U)return _(Pe)(t.value0)(r);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Pe}},ha={bind:function(t){return function(r){if(t instanceof U)return r(t.value0);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Ri}};var To=function(){return{pure:U.create,Apply0:function(){return Ri}}}();var jt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Xt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var yf={map:function(t){return function(r){if(r instanceof jt)return new jt(r.value0);if(r instanceof Xt)return new Xt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[r.constructor.name])}}};var Ra=function(t){return function(r){return function(e){if(e instanceof jt)return t(e.value0);if(e instanceof Xt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},ep=function(){return Ra(x(V.value))(U.create)}();var pu=function(t){return t};var So={map:function(t){return function(r){return t(r)}}};var by={apply:function(t){return function(r){return t(r)}},Functor0:function(){return So}},tS={bind:function(t){return function(r){return r(t)}},Apply0:function(){return by}},ym={pure:pu,Apply0:function(){return by}},Qu={Applicative0:function(){return ym},Bind1:function(){return tS}};var yy=function(t){return Math.min(Math.abs(t),2147483647)},Ay=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},ky=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},gy=function(t){return function(r){return t/r}};var Cy={Ring0:function(){return mf}},Ey={Ring0:function(){return _m}};var su=function(t){return t.mod};var yl={degree:function(t){return 1},div:gy,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return Cy}},xo={degree:yy,div:Ay,mod:ky,CommutativeRing0:function(){return Ey}},Ku=function(t){return t.div};var Ie={mempty:void 0,Semigroup0:function(){return my}};var wt=function(t){return t.mempty},Qe=function(t){return{mempty:function(r){return wt(t)},Semigroup0:function(){return dm(t.Semigroup0())}}};var Am=function(t){return function(){return t}},hy=function(t){return function(r){return function(){return r(t())()}}};var Al=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var Ty=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},ur={Applicative0:function(){return f},Bind1:function(){return Tn}},Tn={bind:hy,Apply0:function(){return km(0)}},f={pure:Am,Apply0:function(){return km(0)}},Sy=Ty("functorEffect","Effect",function(){return{map:Dl(f)}}),km=Ty("applyEffect","Effect",function(){return{apply:ju(ur),Functor0:function(){return Sy(0)}}}),F=Sy(20),nt=km(23),xy=function(t){return{append:ca(nt)(bt(t))}},ge=function(t){return{mempty:Am(wt(t)),Semigroup0:function(){return xy(t.Semigroup0())}}};var Fy=function(t){return function(){return{value:t}}};var Re=function(t){return function(){return t.value}},Oy=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},Sn=function(t){return function(r){return function(){r.value=t}}};var se=Fy,oS=Oy,tc=function(t){return oS(function(r){var e=t(r);return{state:e,value:e}})},Af=function(t){return function(r){return Ar(F)(tc(t)(r))}};var _S=py,wu=function(t){return _S(function(r){var e=t(r);return{state:e,value:e}})},Ni={map:_y};var k={liftST:ly,Monad0:function(){return ur}},$r=function(t){return t.liftST};var pn=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},sS=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},mS=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},np=typeof Array.prototype.fill=="function"?sS:mS,vS=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],i=0,o=a;o!==r;)u[i++]=o.head,o=o.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),Pu=function(t){return t.length};var My=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var wy=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var DS=function(){function t(r,e,n,a,u,i){var o,m,s,l,v,D,c;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),m=u,s=o,l=u;m<o&&s<i;)v=a[m],D=a[s],c=e(r(v)(D)),c>0?(n[l++]=D,++s):(n[l++]=v,++m);for(;m<o;)n[l++]=a[m++];for(;s<i;)n[l++]=a[s++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var kl=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var Py=function(t){return function(r){return t[r]}};var bS=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var Iy={defer:function(t){return function(r){return t(void 0)(r)}}},rc=function(t){return t.defer},Cm=function(t){return function(r){var e=bS("go","Control.Lazy",function(){return rc(t)(function(a){return r(e(25))})}),n=e(25);return n}};var yS=function(){function t(r,e,n,a,u,i){var o,m,s,l,v,D,c;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),m=u,s=o,l=u;m<o&&s<i;)v=a[m],D=a[s],c=e(r(v)(D)),c>0?(n[l++]=D,++s):(n[l++]=v,++m);for(;m<o;)n[l++]=a[m++];for(;s<i;)n[l++]=a[s++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var Uy=function(t){return function(r){return t&&r}},Wy=function(t){return function(r){return t||r}},qy=function(t){return!t};var Du=function(t){return t.not};var nc=function(t){return t.disj},La={ff:!1,tt:!0,implies:function(t){return function(r){return nc(La)(Du(La)(t))(r)}},conj:Uy,disj:Wy,not:qy};var zy=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},Gy=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var M=function(t){return t.empty};var at=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),ac=function(t){return function(r){return t(r.value0)(r.value1)}};var en=function(t){return t.value1};var ti={map:function(t){return function(r){return new at(r.value0,t(r.value1))}}};var Ua=function(t){return t.value0};var RS=function(t){return{append:function(r){return function(e){return Mu(t)(r)(e)}}}};var xl=function(t){return{mempty:tt(t),Semigroup0:function(){return RS(t.Semigroupoid0())}}};var Ln=function(){return lt};var nn=Ln,Ce=Ln;var Om=function(){return function(){return function(t){return Ln()}}};var re=function(t){return t.foldr};var he=function(t){return function(r){return re(t)(N(r.Alt0()))(M(r))}},sn=function(t){return function(r){return function(e){return re(t)(function(){var n=N(r.Alt0());return function(a){return n(e(a))}}())(M(r))}}},ve=function(t){return function(r){return function(e){return re(r)(function(){var n=X(t.Apply0());return function(a){return n(e(a))}}())(S(t)(void 0))}}},_a=function(t){return function(r){return xt(ve(t)(r))}},lp=function(t){return function(r){return ve(t)(r)(tt(rt))}},De=function(t){return t.foldl};var Kr={foldr:function(t){return function(r){return function(e){if(e instanceof V)return r;if(e instanceof U)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof V)return r;if(e instanceof U)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof V)return wt(t);if(e instanceof U)return r(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,e.constructor.name])}}}};var Qy=function(t){return function(r){return function(e){return re(t)(function(n){return function(a){return bt(r.Semigroup0())(e(n))(a)}})(wt(r))}}},Gt={foldr:zy,foldl:Gy,foldMap:function(t){return Qy(Gt)(t)}};var xn=function(t){return t.foldMap};var Ky=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(i){return[a,u,i]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(i){return function(o){return function(m){function s(l,v){switch(v-l){case 0:return i([]);case 1:return u(t)(o(m[l]));case 2:return a(u(r)(o(m[l])))(o(m[l+1]));case 3:return a(a(u(e)(o(m[l])))(o(m[l+1])))(o(m[l+2]));default:var D=l+Math.floor((v-l)/4)*2;return a(u(n)(s(l,D)))(s(D,v))}}return s(0,m.length)}}}}}}();var Bn=function(t){return t.traverse};var oA=function(t){return function(r){return Bn(t)(r)(tt(rt))}},ri={traverse:function(t){return Ky(qt(t.Apply0()))(_(t.Apply0().Functor0()))(S(t))},sequence:function(t){return oA(ri)(t)},Functor0:function(){return Or},Foldable1:function(){return Gt}};var Ml=function(){return kl(at.create)}();var Hm=function(){return Py};var sA=function(t){return[t]};var mA=function(){return My(U.create)(V.value)}();var zm=function(){return wy(U.create)(V.value)}(),Gm=function(t){return function(r){return function(e){return e.length===0?[]:Yt(e)(function(n){return ta()(zm(n)(e))})(mA(t(r))(e))}}};var Ui=function(t){return function(r){return bt(hn)([t])(r)}};var vA=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var ro=function(t){return t.mapWithIndex};var Wi={mapWithIndex:vA,Functor0:function(){return Or}};var $o=function(t){return t.foldrWithIndex};var no=function(t){return t.foldlWithIndex};var ni=function(t){return t.foldMapWithIndex};var qi=function(t){return t.traverseWithIndex};var ao=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var yp=function(t){return function(r){return new ao(r,M(t))}};var Fe=function(){function t(){}return t.value=new t,t}(),cr=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Ap=function(t){return t},Qx=function(t){return new cr(t.value0,t.value1)};var Kx=function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(m,s){if(s instanceof cr&&s.value1 instanceof cr&&s.value1.value1 instanceof cr){a=new cr(s,m),n=s.value1.value1.value1;return}var l=function(D){return D instanceof cr&&D.value1 instanceof cr&&D.value1.value1 instanceof Fe?new cr(t(D.value0),new cr(t(D.value1.value0),Fe.value)):D instanceof cr&&D.value1 instanceof Fe?new cr(t(D.value0),Fe.value):Fe.value},v=function(D){return function(c){var C=D,ut=!1,mt;function vr(nr,Zr){if(nr instanceof cr&&nr.value0 instanceof cr&&nr.value0.value1 instanceof cr&&nr.value0.value1.value1 instanceof cr){C=nr.value1,c=new cr(t(nr.value0.value0),new cr(t(nr.value0.value1.value0),new cr(t(nr.value0.value1.value1.value0),Zr)));return}return ut=!0,Zr}for(;!ut;)mt=vr(C,c);return mt}};return u=!0,v(m)(l(s))}for(;!u;)i=o(a,n);return i}};return r(Fe.value)},kp={map:Kx};var Wa={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(i){var o=u,m=!1,s;function l(v,D){if(D instanceof Fe)return m=!0,v;if(D instanceof cr){o=new cr(D.value0,v),i=D.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[v.constructor.name,D.constructor.name])}for(;!m;)s=l(o,i);return s}};return a(Fe.value)}(),n=De(Wa)(xt(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(m,s){if(s instanceof Fe)return u=!0,m;if(s instanceof cr){a=t(m)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)i=o(a,n);return i}};return r},foldMap:function(t){return function(r){return De(Wa)(function(e){var n=bt(t.Semigroup0())(e);return function(a){return n(r(a))}})(wt(t))}}};var wl={append:function(t){return function(r){return re(Wa)(cr.create)(r)(t)}}};var Jm={append:function(t){return function(r){return new ao(t.value0,bt(wl)(t.value1)(Qx(r)))}}};var yA={alt:bt(wl),Functor0:function(){return kp}},jm=function(){return{empty:Fe.value,Alt0:function(){return yA}}}();var hA=function(t){return t()};var TA=function(t){throw new Error(t)};var SA=function(){return TA};var bF=hA,Qa=function(t){return bF(function(){return SA()(t)})};var Qt=function(){function t(){}return t.value=new t,t}(),sr=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Wr=function(){function t(r,e,n,a,u,i,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i,this.value6=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return new t(r,e,n,a,u,i,o)}}}}}}},t}(),zi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),fi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),Gi=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Mo=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Vi=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Cp=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var FA=function(t){return function(r){return new sr(Qt.value,t,r,Qt.value)}};var EF=function(t){return function(r){var e=rr(t),n=function(a){var u=!1,i;function o(m){if(m instanceof Qt)return u=!0,V.value;if(m instanceof sr){var s=e(r)(m.value1);if(s instanceof yr)return u=!0,new U(m.value2);if(s instanceof tr){a=m.value0;return}a=m.value3;return}if(m instanceof Wr){var l=e(r)(m.value1);if(l instanceof yr)return u=!0,new U(m.value2);var v=e(r)(m.value4);if(v instanceof yr)return u=!0,new U(m.value5);if(l instanceof tr){a=m.value0;return}if(v instanceof br){a=m.value6;return}a=m.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[m.constructor.name])}for(;!u;)i=o(a);return i};return n}};var OA=function(t){return t instanceof Qt};var an=function(t){return function(r){return function(e){var n=t,a=r,u=!1,i;function o(m,s,l){if(s instanceof Fe)return u=!0,l;if(s instanceof cr){if(s.value0 instanceof zi){n=m,a=s.value1,e=new sr(l,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof fi){n=m,a=s.value1,e=new sr(s.value0.value0,s.value0.value1,s.value0.value2,l);return}if(s.value0 instanceof Gi){n=m,a=s.value1,e=new Wr(l,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Mo){n=m,a=s.value1,e=new Wr(s.value0.value0,s.value0.value1,s.value0.value2,l,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Vi){n=m,a=s.value1,e=new Wr(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,l);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,l.constructor.name])}for(;!u;)i=o(n,a,e);return i}}},Rl=function(t){return function(r){return function(e){var n=function(i){return function(o){var m=i,s=!1,l;function v(D,c){if(D instanceof Fe)return s=!0,new sr(c.value0,c.value1,c.value2,c.value3);if(D instanceof cr){if(D.value0 instanceof zi)return s=!0,an(t)(D.value1)(new Wr(c.value0,c.value1,c.value2,c.value3,D.value0.value0,D.value0.value1,D.value0.value2));if(D.value0 instanceof fi)return s=!0,an(t)(D.value1)(new Wr(D.value0.value0,D.value0.value1,D.value0.value2,c.value0,c.value1,c.value2,c.value3));if(D.value0 instanceof Gi){m=D.value1,o=new Cp(new sr(c.value0,c.value1,c.value2,c.value3),D.value0.value0,D.value0.value1,new sr(D.value0.value2,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Mo){m=D.value1,o=new Cp(new sr(D.value0.value0,D.value0.value1,D.value0.value2,c.value0),c.value1,c.value2,new sr(c.value3,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Vi){m=D.value1,o=new Cp(new sr(D.value0.value0,D.value0.value1,D.value0.value2,D.value0.value3),D.value0.value4,D.value0.value5,new sr(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[D.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[D.constructor.name,c.constructor.name])}for(;!s;)l=v(m,o);return l}},a=rr(t),u=function(i){return function(o){var m=i,s=!1,l;function v(D,c){if(c instanceof Qt)return s=!0,n(D)(new Cp(Qt.value,r,e,Qt.value));if(c instanceof sr){var C=a(r)(c.value1);if(C instanceof yr)return s=!0,an(t)(D)(new sr(c.value0,r,e,c.value3));if(C instanceof tr){m=new cr(new zi(c.value1,c.value2,c.value3),D),o=c.value0;return}m=new cr(new fi(c.value0,c.value1,c.value2),D),o=c.value3;return}if(c instanceof Wr){var ut=a(r)(c.value1);if(ut instanceof yr)return s=!0,an(t)(D)(new Wr(c.value0,r,e,c.value3,c.value4,c.value5,c.value6));var mt=a(r)(c.value4);if(mt instanceof yr)return s=!0,an(t)(D)(new Wr(c.value0,c.value1,c.value2,c.value3,r,e,c.value6));if(ut instanceof tr){m=new cr(new Gi(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),D),o=c.value0;return}if(ut instanceof br&&mt instanceof tr){m=new cr(new Mo(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),D),o=c.value3;return}m=new cr(new Vi(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),D),o=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[D.constructor.name,c.constructor.name])}for(;!s;)l=v(m,o);return l}};return u(Fe.value)}}},hF=function(t){return function(r){var e=function(o){return function(m){var s=o,l=!1,v;function D(c,C){if(c instanceof Fe)return l=!0,C;if(c instanceof cr){if(c.value0 instanceof zi&&c.value0.value2 instanceof Qt&&C instanceof Qt)return l=!0,an(t)(c.value1)(new sr(Qt.value,c.value0.value0,c.value0.value1,Qt.value));if(c.value0 instanceof fi&&c.value0.value0 instanceof Qt&&C instanceof Qt)return l=!0,an(t)(c.value1)(new sr(Qt.value,c.value0.value1,c.value0.value2,Qt.value));if(c.value0 instanceof zi&&c.value0.value2 instanceof sr){s=c.value1,m=new Wr(C,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof fi&&c.value0.value0 instanceof sr){s=c.value1,m=new Wr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,C);return}return c.value0 instanceof zi&&c.value0.value2 instanceof Wr?(l=!0,an(t)(c.value1)(new sr(new sr(C,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new sr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof fi&&c.value0.value0 instanceof Wr?(l=!0,an(t)(c.value1)(new sr(new sr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new sr(c.value0.value0.value6,c.value0.value1,c.value0.value2,C)))):c.value0 instanceof Gi&&c.value0.value2 instanceof Qt&&c.value0.value5 instanceof Qt&&C instanceof Qt?(l=!0,an(t)(c.value1)(new Wr(Qt.value,c.value0.value0,c.value0.value1,Qt.value,c.value0.value3,c.value0.value4,Qt.value))):c.value0 instanceof Mo&&c.value0.value0 instanceof Qt&&c.value0.value5 instanceof Qt&&C instanceof Qt?(l=!0,an(t)(c.value1)(new Wr(Qt.value,c.value0.value1,c.value0.value2,Qt.value,c.value0.value3,c.value0.value4,Qt.value))):c.value0 instanceof Vi&&c.value0.value0 instanceof Qt&&c.value0.value3 instanceof Qt&&C instanceof Qt?(l=!0,an(t)(c.value1)(new Wr(Qt.value,c.value0.value1,c.value0.value2,Qt.value,c.value0.value4,c.value0.value5,Qt.value))):c.value0 instanceof Gi&&c.value0.value2 instanceof sr?(l=!0,an(t)(c.value1)(new sr(new Wr(C,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Mo&&c.value0.value0 instanceof sr?(l=!0,an(t)(c.value1)(new sr(new Wr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,C),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Mo&&c.value0.value5 instanceof sr?(l=!0,an(t)(c.value1)(new sr(c.value0.value0,c.value0.value1,c.value0.value2,new Wr(C,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof Vi&&c.value0.value3 instanceof sr?(l=!0,an(t)(c.value1)(new sr(c.value0.value0,c.value0.value1,c.value0.value2,new Wr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,C)))):c.value0 instanceof Gi&&c.value0.value2 instanceof Wr?(l=!0,an(t)(c.value1)(new Wr(new sr(C,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new sr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Mo&&c.value0.value0 instanceof Wr?(l=!0,an(t)(c.value1)(new Wr(new sr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new sr(c.value0.value0.value6,c.value0.value1,c.value0.value2,C),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Mo&&c.value0.value5 instanceof Wr?(l=!0,an(t)(c.value1)(new Wr(c.value0.value0,c.value0.value1,c.value0.value2,new sr(C,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new sr(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof Vi&&c.value0.value3 instanceof Wr?(l=!0,an(t)(c.value1)(new Wr(c.value0.value0,c.value0.value1,c.value0.value2,new sr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new sr(c.value0.value3.value6,c.value0.value4,c.value0.value5,C)))):(l=!0,Qa("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!l;)v=D(s,m);return v}},n=function(o){return function(m){var s=o,l=!1,v;function D(c,C){if(C instanceof sr&&C.value0 instanceof Qt&&C.value3 instanceof Qt)return l=!0,e(c)(Qt.value);if(C instanceof sr){s=new cr(new fi(C.value0,C.value1,C.value2),c),m=C.value3;return}if(C instanceof Wr&&C.value0 instanceof Qt&&C.value3 instanceof Qt&&C.value6 instanceof Qt)return l=!0,e(new cr(new fi(Qt.value,C.value1,C.value2),c))(Qt.value);if(C instanceof Wr){s=new cr(new Vi(C.value0,C.value1,C.value2,C.value3,C.value4,C.value5),c),m=C.value6;return}return l=!0,Qa("The impossible happened in partial function `removeMaxNode`.")}for(;!l;)v=D(s,m);return v}},a=function(o){var m=!1,s;function l(v){if(v instanceof sr&&v.value3 instanceof Qt)return m=!0,{key:v.value1,value:v.value2};if(v instanceof sr){o=v.value3;return}if(v instanceof Wr&&v.value6 instanceof Qt)return m=!0,{key:v.value4,value:v.value5};if(v instanceof Wr){o=v.value6;return}return m=!0,Qa("The impossible happened in partial function `maxNode`.")}for(;!m;)s=l(o);return s},u=rr(t),i=function(o){return function(m){var s=o,l=!1,v;function D(c,C){if(C instanceof Qt)return l=!0,V.value;if(C instanceof sr){var ut=u(r)(C.value1);if(C.value3 instanceof Qt&&ut instanceof yr)return l=!0,new U(new at(C.value2,e(c)(Qt.value)));if(ut instanceof yr){var mt=a(C.value0);return l=!0,new U(new at(C.value2,n(new cr(new zi(mt.key,mt.value,C.value3),c))(C.value0)))}if(ut instanceof tr){s=new cr(new zi(C.value1,C.value2,C.value3),c),m=C.value0;return}s=new cr(new fi(C.value0,C.value1,C.value2),c),m=C.value3;return}if(C instanceof Wr){var vr=function(){return C.value0 instanceof Qt&&C.value3 instanceof Qt&&C.value6 instanceof Qt}(),ut=u(r)(C.value4),nr=u(r)(C.value1);if(vr&&nr instanceof yr)return l=!0,new U(new at(C.value2,an(t)(c)(new sr(Qt.value,C.value4,C.value5,Qt.value))));if(vr&&ut instanceof yr)return l=!0,new U(new at(C.value5,an(t)(c)(new sr(Qt.value,C.value1,C.value2,Qt.value))));if(nr instanceof yr){var mt=a(C.value0);return l=!0,new U(new at(C.value2,n(new cr(new Gi(mt.key,mt.value,C.value3,C.value4,C.value5,C.value6),c))(C.value0)))}if(ut instanceof yr){var mt=a(C.value3);return l=!0,new U(new at(C.value5,n(new cr(new Mo(C.value0,C.value1,C.value2,mt.key,mt.value,C.value6),c))(C.value3)))}if(nr instanceof tr){s=new cr(new Gi(C.value1,C.value2,C.value3,C.value4,C.value5,C.value6),c),m=C.value0;return}if(nr instanceof br&&ut instanceof tr){s=new cr(new Mo(C.value0,C.value1,C.value2,C.value4,C.value5,C.value6),c),m=C.value3;return}s=new cr(new Vi(C.value0,C.value1,C.value2,C.value3,C.value4,C.value5),c),m=C.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[C.constructor.name])}for(;!l;)v=D(s,m);return v}};return i(Fe.value)}},Ta={foldr:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof sr)return re(Ta)(t)(t(e.value2)(re(Ta)(t)(r)(e.value3)))(e.value0);if(e instanceof Wr)return re(Ta)(t)(t(e.value2)(re(Ta)(t)(t(e.value5)(re(Ta)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof sr)return De(Ta)(t)(t(De(Ta)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Wr)return De(Ta)(t)(t(De(Ta)(t)(t(De(Ta)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof Qt)return wt(t);if(e instanceof sr)return bt(t.Semigroup0())(xn(Ta)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value2))(xn(Ta)(t)(r)(e.value3)));if(e instanceof Wr)return bt(t.Semigroup0())(xn(Ta)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value2))(bt(t.Semigroup0())(xn(Ta)(t)(r)(e.value3))(bt(t.Semigroup0())(r(e.value5))(xn(Ta)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[e.constructor.name])}}}},pa={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof sr)return $o(pa)(t)(t(e.value1)(e.value2)($o(pa)(t)(r)(e.value3)))(e.value0);if(e instanceof Wr)return $o(pa)(t)(t(e.value1)(e.value2)($o(pa)(t)(t(e.value4)(e.value5)($o(pa)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof sr)return no(pa)(t)(t(e.value1)(no(pa)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Wr)return no(pa)(t)(t(e.value4)(no(pa)(t)(t(e.value1)(no(pa)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){return function(r){return function(e){if(e instanceof Qt)return wt(t);if(e instanceof sr)return bt(t.Semigroup0())(ni(pa)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value1)(e.value2))(ni(pa)(t)(r)(e.value3)));if(e instanceof Wr)return bt(t.Semigroup0())(ni(pa)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value1)(e.value2))(bt(t.Semigroup0())(ni(pa)(t)(r)(e.value3))(bt(t.Semigroup0())(r(e.value4)(e.value5))(ni(pa)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[e.constructor.name])}}},Foldable0:function(){return Ta}},$A=function(){return $o(pa)(function(t){return function(r){return function(e){return new cr(t,e)}}})(Fe.value)}();var hp=function(){return Qt.value}();var Zm=function(t){return function(r){return function(e){return Yt(e)(en)(hF(t)(r)(e))}}};var Tp=function(t){return function(r){return function(e){return function(n){var a=r(EF(t)(e)(n));if(a instanceof V)return Zm(t)(e)(n);if(a instanceof U)return Rl(t)(e)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var TF=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return function(o){return Tp(t)(function(){var m=Yt(o)(r(o));return function(s){return U.create(m(s))}}())(u)(i)}}};return no(pa)(a)(n)(e)}}}};var MA=function(t){return TF(t)(x)};var Ll=function(t){return t.partitionMap};var Ji=function(t){return t.filterMap};var Bl=function(t){return t.filter};var pc=function(t){return{always:tt(rt),Monoid0:function(){return t}}};var Un={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},wo=function(t){return t.dimap},Ka=function(t){return function(r){return wo(t)(r)(tt(rt))}};var $F=function(t){return function(r){return function(e){return MA(t)(r)(e)}}};var nv=function(t){return $A(t)};var BA=function(t){return FA(t)(void 0)};var av=function(t){return{append:$F(t)}};var UA=function(t){return OA(t)},WA=function(t){return function(r){return function(e){return Rl(t)(r)(void 0)(e)}}};var qA={foldMap:function(t){return function(r){var e=xn(Wa)(t)(r);return function(n){return e(nv(n))}}},foldl:function(t){return function(r){var e=De(Wa)(t)(r);return function(n){return e(nv(n))}}},foldr:function(t){return function(r){var e=re(Wa)(t)(r);return function(n){return e(nv(n))}}}};var uv=hp;var HA=function(t){return{mempty:uv,Semigroup0:function(){return av(t)}}};var Sp=function(t){return function(r){return function(e){return Zm(t)(r)(e)}}};function zA(t){return function(r){return function(){return setTimeout(r,t)}}}function GA(t){return function(){clearTimeout(t)}}var xp=zA;var wF={eq:function(t){return function(r){return t===r}}},Fp={compare:function(t){return function(r){return rr(Xe)(t)(r)}},Eq0:function(){return wF}};var Wl=GA;var uo=function(t){return t.sampleOn};var Oe=function(t){return t.keepLatest},Iu=function(t){return t.fold};var ql=function(t){return function(r){return function(e){return function(n){return Ji(t.Filterable1())(en)(Iu(t)(function(a){return function(u){return _(ti)(S(To))(r(a)(u.value0))}})(e)(new at(n,V.value)))}}}},Op=function(t){return function(r){var e=function(n){return function(a){if(a instanceof V)return new U({now:n,last:V.value});if(a instanceof U)return new U({now:n,last:new U(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,a.constructor.name])}};return Ji(t.Filterable1())(tt(rt))(Iu(t)(e)(r)(V.value))}},Hl=function(t){return t.fix};var On=function(t){return function(r){return function(e){return N(t.Plus0().Alt0())(uo(t)(r)(e))(uo(t)(e)(_(t.Filterable1().Functor1())(Qf)(r)))}}},Y=function(t){return t.bang};function iv(t){return function(r){return t===r}}var fv=iv;var NF=function(t){return t};var Ot=function(t){return function(r){return t(r)}},LF=function(t){return function(r){return function(e){return function(n){return function(a){return H(t.Monad0().Bind1())($r(t)(la(V.value)))(function(u){return H(t.Monad0().Bind1())(e(function(i){return $r(t)(Ar(Ni)(ja(new U(i))(u)))}))(function(i){return H(t.Monad0().Bind1())(n(function(o){return H(t.Monad0().Bind1())($r(t)(Nn(u)))(ve(r)(Kr)(function(m){return a(o(m))}))}))(function(o){return S(r)(X(r.Apply0())(i)(o))})})})}}}}},Bt=NF,BF=function(t){return function(r){return function(e){return H(t.Monad0().Bind1())($r(t)(la(V.value)))(function(n){return H(t.Monad0().Bind1())(r(function(a){return ae(jr)(t.Monad0().Bind1())(H(t.Monad0().Bind1())($r(t)(Nn(n)))(lp(t.Monad0().Applicative0())(Kr)))(function(){return H(t.Monad0().Bind1())(Ot(a)(e))(function(u){return $r(t)(Ar(Ni)(ja(new U(u))(n)))})})}))(function(a){return S(t.Monad0().Applicative0())(ae(jr)(t.Monad0().Bind1())(H(t.Monad0().Bind1())($r(t)(Nn(n)))(lp(t.Monad0().Applicative0())(Kr)))(function(){return a}))})})}}},g={map:function(t){return function(r){return function(e){return r(function(n){return e(t(n))})}}}};var UF=function(t){return function(r){return function(e){return function(n){return function(a){return H(t.Monad0().Bind1())($r(t)(la(n)))(function(u){return e(function(i){return H(t.Monad0().Bind1())($r(t)(wu(r(i))(u)))(a)})})}}}}},zl=function(t){return function(r){return function(e){return function(n){return e(function(a){var u=r(a);if(u instanceof U)return n(u.value0);if(u instanceof V)return S(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 126, column 13 - line 128, column 27): "+[u.constructor.name])})}}}},cv=function(t){return function(r){return zl(t)(function(e){var n=r(e);if(n)return new U(e);if(!n)return V.value;throw new Error("Failed pattern match at FRP.Event (line 84, column 13 - line 86, column 25): "+[n.constructor.name])})}},Ru=function(t){return function(r){return Bt(function(e){return function(){var a=se(wt(HA(Fp)))(),u=Ot(r)(function(i){return function(){var m=se(V.value)(),s=xp(t)(function(){e(i)();var v=Re(m)();return Yt(S(f)(void 0))(function(D){return Af(Sp(Fp)(D))(a)})(v)()})();return Sn(new U(s))(m)(),Af(bt(av(Fp))(BA(s)))(a)()}})();return function(){var o=Re(a)();return _a(f)(qA)(o)(Wl)(),u()}}})}};var Gl=function(t){return function(r){return H(t.Monad0().Bind1())($r(t)(la([])))(function(e){return S(t.Monad0().Applicative0())({event:function(n){return H(r.Monad0().Bind1())($r(r)(wu(function(a){return bt(hn)(a)([n])})(e)))(function(){return S(r.Monad0().Applicative0())(H(r.Monad0().Bind1())($r(r)(wu(Gm(fv)(n))(e)))(function(){return S(r.Monad0().Applicative0())(void 0)}))})},push:function(n){return H(r.Monad0().Bind1())($r(r)(Nn(e)))(ve(r.Monad0().Applicative0())(Gt)(function(a){return a(n)}))}})})}},WF=function(t){return function(r){return function(e){return function(n){return H(r.Bind1())(Gl(t)(t))(function(a){var u=e(a.event);return H(r.Bind1())(Ot(u.input)(a.push))(function(i){return H(r.Bind1())(Ot(u.output)(n))(function(o){return S(r.Applicative0())(X(r.Bind1().Apply0())(i)(o))})})})}}}},ji=function(t){return function(r){return function(e){return Bt(function(n){return H(t.Monad0().Bind1())(Gl(t)(t))(function(a){return ae(jr)(t.Monad0().Bind1())(n(e(a.event)))(function(){return Ot(r)(a.push)})})})}}},jA=function(t){return{compact:zl(t)(tt(rt)),separate:function(r){return{left:zl(t)(function(e){if(e instanceof jt)return new U(e.value0);if(e instanceof Xt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 67, column 13 - line 69, column 33): "+[e.constructor.name])})(r),right:zl(t)(function(e){if(e instanceof Xt)return new U(e.value0);if(e instanceof jt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 74, column 13 - line 76, column 32): "+[e.constructor.name])})(r)}}}},bu=function(t){return{filter:cv(t),filterMap:zl(t),partition:function(r){return function(e){return{yes:cv(t)(r)(e),no:cv(t)(function(){var n=Du(La);return function(a){return n(r(a))}}())(e)}}},partitionMap:function(r){return function(e){return{left:Ji(bu(t))(function(){var n=Ra(U.create)(x(V.value));return function(a){return n(r(a))}}())(e),right:Ji(bu(t))(function(n){return ep(r(n))})(e)}}},Compactable0:function(){return jA(t)},Functor1:function(){return g}}},We=function(t){return function(r){return Bt(function(e){return H(t.Monad0().Bind1())(Gl(t)(t))(function(n){return ae(jr)(t.Monad0().Bind1())(e(r(n.push)(n.event)))(function(){return S(t.Monad0().Applicative0())(S(t.Monad0().Applicative0())(void 0))})})})}},$t=function(t){return function(r){return function(e){return _(t.Apply0().Functor0())(function(n){return S(t)(void 0)})(e(r))}}},I=function(t){return{alt:function(r){return function(e){return function(n){return qt(t.Apply0())(_(t.Apply0().Functor0())(function(a){return function(u){return X(t.Apply0())(a)(u)}})(r(n)))(e(n))}}},Functor0:function(){return g}}},h=function(t){return{empty:function(r){return S(t)(S(t)(void 0))},Alt0:function(){return I(t)}}},T=function(t){return{fold:UF(t),keepLatest:BF(t),sampleOn:LF(t)(t.Monad0().Applicative0()),fix:WF(t)(t.Monad0()),bang:$t(t.Monad0().Applicative0()),Plus0:function(){return h(t.Monad0().Applicative0())},Filterable1:function(){return bu(t.Monad0().Applicative0())}}};var $p="_____$__$_$$_vbus";function lv(t){return t[$p]=$p,t}function _v(t){return()=>{for(let r in t)delete t[r]}}function pv(t){return()=>{let r=(u,i,o,m)=>{let s=Object.keys(m);for(var l=0;l<s.length;l++)if(m[s[l]]instanceof Object&&m[s[l]][$p]===$p){let v={},D={};r(u,v,D,m[s[l]]),i[s[l]]=v,o[s[l]]=D}else{let v=`${Math.random()}`;u[v]={},i[s[l]]=D=>()=>{let c=Object.keys(u[v]);for(var C=0;C<c.length;C++)u[v][c[C]](D)()},o[s[l]]=D=>()=>{let c=`${Math.random()}`;return u[v][c]=D,()=>{delete u[v][c]}}}},e={},n={},a={};return r(e,n,a,t),{p:n,e:a,s:e}}}function Vl(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var QA=function(t){return function(){return function(){return function(r){return function(e){return function(n){return Vu(Ae(t)(r))(e)(n)}}}}}};var KA=function(){return function(){return function(t){return function(r){return Vl(t,r)}}}},Jl=function(t){return function(){return function(){return function(r){return function(e){return function(n){return Vu(Ae(t)(r))(e)(n)}}}}}},ci=function(t){return function(){return function(r){return function(e){return Pa(Ae(t)(r))(e)}}}};var Wn={vb:function(t){return function(r){return function(e){return{}}}}},Mp=function(t){return t.vb},Ya=function(){return function(t){return function(r){return function(e){return function(n){var a=Mp(r)(d.value)(d.value)(d.value);return Bt(function(u){return H(t.Monad0().Bind1())(pv(a))(function(i){return ae(jr)(t.Monad0().Bind1())(u(n(i.p)(i.e)))(function(){return S(t.Monad0().Applicative0())(_v(i.s))})})})}}}}},Nu=function(t){return function(){return function(){return function(){return function(r){return function(e){return function(){return function(){return function(){return function(){return{vb:function(n){return function(a){return function(u){return Jl(t)()()(d.value)(lv(Mp(r)(d.value)(d.value)(d.value)))(Mp(e)(d.value)(d.value)(d.value))}}}}}}}}}}}}}},ue=function(t){return function(){return function(){return function(r){return function(){return function(){return function(){return function(){return{vb:function(e){return function(n){return function(a){return Jl(t)()()(d.value)(void 0)(Mp(r)(d.value)(d.value)(d.value))}}}}}}}}}}}};var Ki=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),mv=function(){function t(){}return t.value=new t,t}();var sc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),mc=function(){function t(){}return t.value=new t,t}(),vv=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),wp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Ef=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),hf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),P=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var YA=function(t){return t};var Pp={eq:function(t){return function(r){return t instanceof Ki&&r instanceof Ki?t.value0===r.value0:t instanceof mv&&r instanceof mv}}};var W=function(t){return new Ef(t)},et=function(t){return new hf(t)},Ip=function(t){return new wp(t)};var Po=function(t){return t.reflectType};var jl={map:function(t){return function(r){return _(Or)(t)(r)}}};var XF=function(t){return Po(t)},Tf=function(){return function(t){return t}};var ek=function(t){return[t]};var nk=function(){return function(){return function(){return function(){return function(){return function(t){return function(r){return function(e){return e[XF(t)(r)]}}}}}}}};var Dv=[];var Sf=function(){return function(){return function(t){return function(r){return Ui(t)(r)}}}};function ak(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var Io={};function dv(t){return t()}function uk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function ok(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function ik(t){return function(r){return function(e){return function(n){var a=e;function u(o){return function(m){return r(m)(o)(n[o])}}for(var i in n)hasOwnProperty.call(n,i)&&(a=t(a)(u(i)));return a}}}}function Xl(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var QF=Object.keys||Xl(function(t){return function(){return t}});function bv(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var yv=function(t){return function(r){return function(){return delete r[t],r}}};var Av=Xl(function(t){return function(r){return r}});var nO=ak;var ck=function(t){return function(r){return dv(function(){var n=nO(r)();return t(n)(),n})}};var lk=function(t){return function(r){return ok(r,t)}};var _i=function(t){return function(r){return ck(bv(t)(r))}},Np={map:function(t){return function(r){return uk(r,t)}}},aO={mapWithIndex:lk,Functor0:function(){return Np}},kv=function(){return lt};var Lp=ik(Qf),_k=function(t){return function(r){return Lp(function(e){return function(n){return function(a){return bt(t.Semigroup0())(e)(r(n)(a))}}})(wt(t))}},Ql={foldl:function(t){return Lp(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return re(Gt)(t)(r)(Av(e))}}},foldMap:function(t){return function(r){return _k(t)(x(r))}}},pk={foldlWithIndex:function(t){return Lp(xt(t))},foldrWithIndex:function(t){return function(r){return function(e){return re(Gt)(ac(t))(r)(Xl(at.create)(e))}}},foldMapWithIndex:function(t){return _k(t)},Foldable0:function(){return Ql}},uO={traverseWithIndex:function(t){return function(r){return function(e){return Lp(function(n){return function(a){return function(u){return qt(t.Apply0())(_(t.Apply0().Functor0())(xt(_i(a)))(n))(r(a)(u))}}})(S(t)(Io))(e)}}},FunctorWithIndex0:function(){return aO},FoldableWithIndex1:function(){return pk},Traversable2:function(){return vc}},vc={traverse:function(t){var r=qi(uO)(t);return function(e){return r(x(e))}},sequence:function(t){return Bn(vc)(t)(tt(rt))},Functor0:function(){return Np},Foldable1:function(){return Ql}};var gv=function(t){return ck(yv(t))};var sk=function(){function t(){}return t.value=new t,t}(),Cv=function(){function t(){}return t.value=new t,t}(),oO=function(){function t(){}return t.value=new t,t}();var Ev=function(t){return function(r){return function(e){var n=function(a){var u=function(i){return function(o){return new at(o+1|0,new at(i,o))}};return ql(T(t))(u)(a)(0)};return new wp(Oe(T(t))(ji(t)(n(e))(function(a){return _(g)(function(u){return N(I(t.Monad0().Applicative0()))(Y(T(t))(new sc(r(u.value0))))(_(g)(x(mc.value))(Bl(bu(t.Monad0().Applicative0()))(function(){var i=Zt(Pi)(u.value1+1|0);return function(o){return i(en(o))}}())(a)))})(a)})))}}};var oo=function(t){return function(r){return function(e){return function(n){return function(a){var u=function(i){return i(n)(a)};return function(i){if(i instanceof Ef)return sn(Gt)(h(t))(oo(t)(r)(e)(n)(a))(i.value0);if(i instanceof hf)return Oe(T(r))(_(g)(oo(t)(r)(e)(n)(a))(i.value0));if(i instanceof P)return u(e.toElt(i.value0));if(i instanceof wp)return Bt(function(o){return H(r.Monad0().Bind1())($r(r)(la(Io)))(function(m){return H(r.Monad0().Bind1())(Ot(i.value0)(function(s){return H(r.Monad0().Bind1())(e.ids(a))(function(l){return H(r.Monad0().Bind1())($r(r)(la(S(t)(void 0))))(function(v){return H(r.Monad0().Bind1())(e.ids(a))(function(D){return H(r.Monad0().Bind1())($r(r)(la(S(t)(void 0))))(function(c){return H(r.Monad0().Bind1())($r(r)(la([])))(function(C){return H(r.Monad0().Bind1())($r(r)(la(S(t)(void 0))))(function(ut){return H(r.Monad0().Bind1())(_(t.Apply0().Functor0())(Ki.create)(e.ids(a)))(function(mt){return H(r.Monad0().Bind1())($r(r)(la(sk.value)))(function(vr){return H(r.Monad0().Bind1())(Ot(s)(function(nr){return H(r.Monad0().Bind1())($r(r)(Nn(vr)))(function(Zr){return nr instanceof vv&&Zr instanceof Cv?H(r.Monad0().Bind1())($r(r)(Nn(C)))(ve(t)(Gt)(function(){var le=e.doLogic(nr.value0)(a);return function(pt){return o(le(pt))}}())):nr instanceof mc&&Zr instanceof Cv?ae(jr)(r.Monad0().Bind1())(Ar(t.Apply0().Functor0())($r(r)(ja(oO.value)(vr))))(function(){var le=X(t.Apply0())(X(t.Apply0())(X(t.Apply0())(X(t.Apply0())(H(r.Monad0().Bind1())($r(r)(Nn(C)))(ve(t)(Gt)(function(pt){return _a(t)(Kr)(n.parent)(function(Dr){return o(e.disconnectElement(a)({id:pt,parent:Dr,scope:mt}))})})))(Ja(r.Monad0().Bind1())($r(r)(Nn(v)))))(Ja(r.Monad0().Bind1())($r(r)(Nn(c)))))(Ar(t.Apply0().Functor0())($r(r)(wu(gv(l))(m)))))(Ar(t.Apply0().Functor0())($r(r)(wu(gv(D))(m))));return X(t.Apply0())(Ar(t.Apply0().Functor0())($r(r)(ja(le)(ut))))(le)}):nr instanceof sc&&Zr instanceof sk?ae(jr)(r.Monad0().Bind1())(Ar(t.Apply0().Functor0())($r(r)(ja(Cv.value)(vr))))(function(){return H(r.Monad0().Bind1())(Ot(oo(t)(r)(e)({parent:n.parent,scope:mt,raiseId:function(le){return Ar(t.Apply0().Functor0())($r(r)(wu(bt(hn)([le]))(C)))}})(a)(nr.value0))(o))(function(le){return ae(jr)(r.Monad0().Bind1())(Ar(t.Apply0().Functor0())($r(r)(wu(_i(D)(le))(m))))(function(){return Ar(t.Apply0().Functor0())($r(r)(ja(le)(c)))})})}):S(t)(void 0)})}))(function(nr){return ae(jr)(r.Monad0().Bind1())(Ar(t.Apply0().Functor0())($r(r)(ja(nr)(v))))(function(){return ae(jr)(r.Monad0().Bind1())(Ar(t.Apply0().Functor0())($r(r)(wu(_i(l)(nr))(m))))(function(){return Ja(r.Monad0().Bind1())($r(r)(Nn(ut)))})})})})})})})})})})})}))(function(s){return S(t)(ae(jr)(r.Monad0().Bind1())(H(r.Monad0().Bind1())($r(r)(Nn(m)))(De(Ql)(X(t.Apply0()))(S(t)(void 0))))(function(){return s}))})})});throw new Error("Failed pattern match at Bolson.Control (line 521, column 17 - line 604, column 20): "+[i.constructor.name])}}}}}},iO=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){var o=function(m){return function(s){return Bt(function(l){return H(t.Monad0().Bind1())(zb(_(Or)(x(""))(Tf()(u))))(function(v){var D=he(Gt)(h(t.Monad0().Applicative0()))(ro(Wi)(function(c){return Cm(Iy)(function(C){return function(ut){return ut instanceof P?function(mt){return mt({parent:V.value,scope:e(m.scope),raiseId:function(vr){return Gb(c)(vr)(v)}})(s)}(a.toElt(ut.value0)):C(a.wrapElt(ut))}})})(Tf()(u)));return H(t.Monad0().Bind1())(Ot(D)(l))(function(c){return H(t.Monad0().Bind1())($r(t)(la(S(t.Monad0().Applicative0())(void 0))))(function(C){return H(t.Monad0().Bind1())(_(t.Monad0().Bind1().Apply0().Functor0())(lt)(Vb(v)))(function(ut){var mt=_(jl)(function(nr){return function(Zr){return new P(a.fromEltO1(function(le){return function(pt){return Bt(function(Dr){return ae(jr)(t.Monad0().Bind1())(le.raiseId(nr))(function(){return ae(jr)(t.Monad0().Bind1())(_a(t.Monad0().Applicative0())(Kr)(le.parent)(function(Tr){return Dr(a.giveNewParent(pt)({id:nr,parent:Tr,scope:le.scope})(Zr))}))(function(){return S(t.Monad0().Applicative0())(S(t.Monad0().Applicative0())(void 0))})})})}}))}})(ut),vr=oo(t.Monad0().Applicative0())(t)(n)(m)(s)(i(mt)(lt));return H(t.Monad0().Bind1())(Ot(vr)(l))(function(nr){return ae(jr)(t.Monad0().Bind1())(Ar(t.Monad0().Bind1().Apply0().Functor0())($r(t)(ja(nr)(C))))(function(){return S(t.Monad0().Applicative0())(ae(jr)(t.Monad0().Bind1())(c)(function(){return ae(jr)(t.Monad0().Bind1())(Rn(t.Monad0().Applicative0())(!r)(_a(t.Monad0().Applicative0())(Gt)(Tf()(ut))(function(Zr){return l(a.deleteFromCache(s)({id:Zr}))})))(function(){return Ja(t.Monad0().Bind1())($r(t)(Nn(C)))})}))})})})})})})})}};return new P(a.fromEltO2(o))}}}}}}}};var hv=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return iO()(t)(!1)(tt(rt))(r)(e)(n)(a)}}}}}};var mk=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return Bt(function(o){return H(t.Monad0().Bind1())($r(t)(la(V.value)))(function(m){var s=n(new P(e.fromElt(function(l){return function(v){return Bt(function(D){return ae(jr)(t.Monad0().Bind1())(H(t.Monad0().Bind1())($r(t)(Nn(m)))(function(c){if(c instanceof V)return S(t.Monad0().Applicative0())(void 0);if(c instanceof U)return _a(t.Monad0().Applicative0())(Kr)(l.parent)(function(C){return Rn(t.Monad0().Applicative0())(c.value0!==C)(X(t.Monad0().Bind1().Apply0())(l.raiseId(c.value0))(D(e.connectToParent(i)({id:c.value0,parent:C}))))});throw new Error("Failed pattern match at Bolson.Control (line 630, column 36 - line 637, column 16): "+[c.constructor.name])}))(function(){return S(t.Monad0().Applicative0())(S(t.Monad0().Applicative0())(void 0))})})}})));return Ot(oo(t.Monad0().Applicative0())(t)(r)({parent:u.parent,scope:u.scope,raiseId:function(l){return ae(jr)(t.Monad0().Bind1())(u.raiseId(l))(function(){return Ar(t.Monad0().Bind1().Apply0().Functor0())($r(t)(ja(new U(l))(m)))})}})(i)(s))(o)})})}};return new P(e.fromElt(a))}}}};var fO=function(t){return t},Kl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Yl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),cO=function(t){return t},Up=Ln(),b=cO;var q=function(){return Kl.create}();var ct=function(){return Yl.create}(),Yr=function(){var t=_(fa)(_(F)(x(!0)));return function(r){return fO(t(r))}}(),Z=function(t){return t.attr};var _O=function(t){return t.makeText},pO=function(t){return function(r){return function(e){return _(g)(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},sO=function(t){return function(r){return function(e){return _(g)(function(n){return function(a){if(a.value instanceof Kl)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof Yl)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 75, column 26 - line 77, column 45): "+[a.value.constructor.name])}(Up(n))})(e)}}},mO=function(t){return t.makeElement},vO=function(t){return t.attributeParent},un=function(t){return function(r){var e=function(n){return function(a){return Bt(function(u){return H(t.Bind1())(a.ids)(function(i){return ae(jr)(t.Bind1())(n.raiseId(i))(function(){return _(t.Bind1().Apply0().Functor0())(X(t.Bind1().Apply0())(u(a.deleteFromCache({id:i}))))(Ot(he(Gt)(h(t.Applicative0()))([$t(t.Applicative0())(_O(a)({id:i,parent:n.parent,scope:n.scope})),pO(a)(i)(r)]))(u))})})})}};return new P(e)}},oe=function(t){return function(r){return un(t)($t(t.Applicative0())(r))}},vk=function(t){return oo(t.MonadST5().Monad0().Applicative0())(t.MonadST5())({doLogic:function(r){return function(e){return function(n){return e.sendToTop({id:n})}}},ids:function(){var r=Ce();return function(e){return function(n){return n.ids}(r(e))}}(),disconnectElement:function(r){return function(e){return r.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:Zt(Pp)})}},toElt:function(r){return r}})},DO=function(t){return function(r){return function(e){return function(n){return Bt(function(a){return H(t.MonadST5().Monad0().Bind1())(n.ids)(function(u){return Ot(N(I(t.MonadST5().Monad0().Applicative0()))($t(t.MonadST5().Monad0().Applicative0())(n.makeRoot({id:u,root:r})))(vk(t)({parent:new U(u),scope:new Ki("rootScope"),raiseId:function(i){return S(t.MonadST5().Monad0().Applicative0())(void 0)}})(n)(e)))(a)})})}}}};var Dk=function(t){return function(r){return function(e){return DO(t)(r)(new Ef(e))}}},j=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return Bt(function(o){return H(t.MonadST5().Monad0().Bind1())(i.ids)(function(m){return ae(jr)(t.MonadST5().Monad0().Bind1())(u.raiseId(m))(function(){return _(t.MonadST5().Monad0().Bind1().Apply0().Functor0())(X(t.MonadST5().Monad0().Bind1().Apply0())(o(i.deleteFromCache({id:m}))))(Ot(N(I(t.MonadST5().Monad0().Applicative0()))(he(Gt)(h(t.MonadST5().Monad0().Applicative0()))(bt(hn)([$t(t.MonadST5().Monad0().Applicative0())(mO(i)({id:m,parent:u.parent,scope:u.scope,tag:r})),sO(i)(m)(e)])(Yt([])(function(s){return[$t(t.MonadST5().Monad0().Applicative0())(vO(i)({id:m,parent:s}))]})(u.parent))))(vk(t)({parent:new U(m),scope:u.scope,raiseId:function(s){return S(t.MonadST5().Monad0().Applicative0())(void 0)}})(i)(n)))(o))})})})}};return a}}}};var EO=function(){return function(){return function(){return function(t){return function(r){return function(e){return ll(e.type)(t)?Pa(e.type)(t)(e.value):r(e)}}}}}};var ie=function(){return function(t){return function(r){return function(e){return{type:Ae(t)(r),value:e}}}}};var Ak=function(t){return Qa("Data.Variant: pattern match failure ["+(t.type+"]"))},qe=function(){return function(){return function(){return function(t){return EO()()()(t)(Ak)}}}};function kk(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function gk(t){return function(r){return function(e){return e[t]=r,e}}}var Wp=Yo;var Sv=function(){return function(){return function(t){return function(r){return function(e){return function(n){return gk(Ae(t)(r))(e)(n)}}}}}};var qp=rt,xv=function(t){return function(r){return t(kk(r))}},Ck=xt(xv)({});var p={Always0:function(){return pc(ge(Ie))},Always1:function(){return pc(ge(ge(Ie)))},Always2:function(){return pc(ge(Ie))},Always3:function(){return pc(xl(rt))},Always4:function(){return pc(xl(rt))},MonadST5:function(){return k}};var me=function(){function t(){}return t.value=new t,t}();var de={attr:function(t){return function(r){return b({key:"click",value:ct(r)})}}};var Vt=function(){function t(){}return t.value=new t,t}();var Hp={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var hk={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var st={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var Tk={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}},Dc={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var Ov={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var Sk={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var $v=function(t){return function(r){return function(e){return new P(j(t)("a")(r)(W(e)))}}};var Sr=function(t){return function(r){return function(e){return new P(j(t)("div")(r)(W(e)))}}},qr=function(t){return Sr(t)(M(h(t.MonadST5().Monad0().Applicative0())))};var bc=function(t){return function(r){return function(e){return new P(j(t)("span")(r)(W(e)))}}},Mv=function(t){return bc(t)(M(h(t.MonadST5().Monad0().Applicative0())))};var Fk=(t,r,e,n)=>{t(a=>n.units[a].main.appendChild(n.units[r].main))(e)},Ok=t=>r=>()=>{r.units[t.id].main.parentNode||r.units[t.parent].main.appendChild(r.units[t.id].main)},$k=t=>r=>e=>()=>{var n,a=r.id;e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(a),t&&r.parent.value0&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:n}:e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:document.createElement(r.tag)}},Mk=t=>r=>e=>n=>()=>{var a=e.id,u;n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(a),t&&e.parent.value0&&(u=document.body.querySelectorAll("[data-deku-ssr-"+e.parent.value0+"]").item(0))?n.units[a]={main:u.childNodes[0],parent:e.parent,scope:e.scope}:(n.units[a]={main:document.createTextNode(""),parent:e.parent,scope:e.scope},Fk(r,a,e.parent,n))};function wv(){return{units:{},scopes:{}}}var wk=t=>r=>e=>()=>{var n=r.id,a=r.value;t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":e.units[n].main.setAttribute(r.key,a)},Pk=t=>r=>e=>()=>{var n=r.id,a=r.value;if(t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=i=>a(i)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}},Ik=t=>r=>()=>{var e=t.id;r.units[e].main.nodeValue=t.text},Rk=t=>r=>e=>n=>()=>{var a,u,i=e.id,o=e.html,m=e.verb,s=e.cache,l=e.parent,v=e.scope,D=e.pxScope;if(t&&e.parent.value0&&(a=document.body.querySelectorAll("[data-deku-ssr-"+i+"]").item(0)))n.units[i]={listeners:{},scope:v,parent:l,main:a};else{let C=Object.entries(s);for(var c=0;c<C.length;c++){let ut=C[c][0];C[c][1]===!0?o=o.replace(m+ut+m,'data-deku-attr-internal="'+ut+'"'):o=o.replace(m+ut+m,'<span style="display:contents;" data-deku-elt-internal="'+ut+'"></span>')}u=document.createElement("div"),u.innerHTML=o.trim(),n.units[i]={listeners:{},scope:v,parent:l,main:u.firstChild}}n.scopes[v]||(n.scopes[v]=[]),n.scopes[v].push(i),u||(u=a),u.querySelectorAll("[data-deku-attr-internal]").forEach(function(C){var ut=C.getAttribute("data-deku-attr-internal");let mt=ut+D;n.units[mt]={listeners:{},main:C,scope:v},n.scopes[v].push(mt)}),u.querySelectorAll("[data-deku-elt-internal]").forEach(function(C){var ut=C.getAttribute("data-deku-elt-internal");let mt=ut+D;n.units[ut+D]={listeners:{},main:C,scope:v},n.scopes[v].push(mt)}),a||Fk(r,i,l,n)},Nk=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root}},Lk=t=>r=>()=>{var e=t.id,n=t.parent;r.units[e].containingScope=t.scope,r.units[n].main.prepend(r.units[e].main)},Bk=t=>r=>()=>{var e=t.id;r.units[e].noop||r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope)||r.units[e].main.remove()},Uk=t=>r=>()=>{delete r.units[t.id]},Wk=t=>r=>()=>{var e=t.id;r.units[e].main.parentNode.prepend(r.units[e].main)};var qk=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},Gr=function(t){return t};var Pv=function(t){return function(r){return Math.pow(t,r)|0}};var zp=isFinite;var Zl=Math.floor;var Yi=function(t){return function(r){return Math.pow(t,r)}},t_=function(t){return function(r){return t%r}},Gp=Math.round;var Vp=Math.sin;var Zi=3.141592653589793;var yc=function(){return qk(U.create)(V.value)}(),zk=function(t){if(!zp(t))return 0;if(t>=Gr(Kn(df)))return Kn(df);if(t<=Gr(Yn(df)))return Yn(df);if(ne)return Ea(0)(yc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Gk=function(t){return zk(Gp(t))};var r_=function(t){return zk(Zl(t))};var Uu=Math.random;var e_=function(t){return function(r){return function(){var n=Uu(),a=(Gr(r)-Gr(t)+1)*n+Gr(t);return r_(a)}}};var Vk=function(t){return t};var IO=1,Jp=2147483647,RO=function(){return Jp-1|0}(),kc=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,i=su(xo)(a)(u),o=i<e;return o?i+n|0:i}}};return r(IO)(RO)(t)};var NO=0,LO=48271,Jk=function(t){return function(r){return ta()(yc(t_(Gr(LO)*Gr(r)+Gr(t))(Gr(Jp))))}},jk=Jk(NO);var VO=function(){function t(i){this.fn=i}var r={},e=function(i,o){this.head=i,this.tail=o};function n(i){return new e(i,r)}function a(i){return function(o){return new e(i,o)}}function u(i){for(var o=[],m=i;m!==r;)o.push(m.head),m=m.tail;return o}return function(i){return function(o){return function(m){var s=function(v,D){return i(o(a)(m(v)))(D)},l=function(v,D,c){if(D===0)return v;var C=c[D-1];return new t(function(){var ut=l(s(C,v),D-1,c);return ut})};return function(v){for(var D=o(n)(m(v[v.length-1])),c=l(D,v.length-1,v);c instanceof t;)c=c.fn();return o(u)(c)}}}}}();var Zk=function(t){return t};var tg=hn;var rg=Gt;var ag=Zk,a_=function(t){return t};var u_=function(t){return ag(sA(t))};var gc=function(t){if(Pu(t)>0)return new U(ag(t));if(ne)return V.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var ug=function(t){return function(r){return t(a_(r))}};var og=ug(Pu);var ig=function(){return ug(Hm())};var pi=function(t){return t.state};function Lo(t){return new Error(t)}function Of(t){return function(){throw t}}function Qp(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var fo=function(t){return t.throwError};var h$={throwError:Of,Monad0:function(){return ur}};var Vv={catchError:xt(Qp),MonadThrow0:function(){return h$}};var si=function(t){return t.catchError};var i_=function(t){return function(r){return si(t)(_(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Xt.create)(r))(function(){var e=S(t.MonadThrow0().Monad0().Applicative0());return function(n){return e(jt.create(n))}}())}};var fe={liftEffect:tt(rt),Monad0:function(){return ur}},be=function(t){return t.liftEffect};var jv=function(t){return{map:function(r){return function(e){return function(n){return _(t)(function(a){return new at(r(a.value0),a.value1)})(e(n))}}}}};var Xv=function(t){return{Applicative0:function(){return Kv(t)},Bind1:function(){return Qv(t)}}},Qv=function(t){return{bind:function(r){return function(e){return function(n){return H(t.Bind1())(r(n))(function(a){var u=e(a.value0);return u(a.value1)})}}},Apply0:function(){return rs(t)}}},rs=function(t){return{apply:ju(Xv(t)),Functor0:function(){return jv(t.Bind1().Apply0().Functor0())}}},Kv=function(t){return{pure:function(r){return function(e){return S(t.Applicative0())(new at(r,e))}},Apply0:function(){return rs(t)}}};var mg=function(t){return{state:function(r){var e=S(t.Applicative0());return function(n){return e(r(n))}},Monad0:function(){return Xv(t)}}};var Dg=function(t){return function(r){var e=t(r);return e.value0}};var w$=function(t){return t};var bg=function(){var t=function(r){return new at(Vk(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=jk(r.newSeed),e}())};return pi(mg(Qu))(t)}();var co=jv(So),yg=_(co)(function(t){return Gr(t)/Gr(Jp)})(bg);var hc=function(t){return Dg(w$(t))};var Mf=Qv(Qu);var wf=rs(Qu),dg=function(t){return function(r){var e=Gr(r),n=Gr(t),a=function(o){return n+t_(o)(e-n+1)},u=_(co)(Gr)(bg),i=qt(wf)(_(co)(Br(ga))(u))(_(co)(In(ga)(2))(u));return _(co)(function(o){return r_(a(o))})(i)}},Yv=function(t){return function(r){var e=t<=r;return e?dg(t)(r):dg(r)(t)}};var p_=Kv(Qu);var Zv=function(t){return H(Mf)(Yv(0)(og(t)-1|0))(function(r){return S(p_)(ig()(t)(r))})};var m_=function(t){return t.arbitrary};var Ag={arbitrary:yg};var kg=function(){return{arbitrary:Yv(-1e6)(1e6)}}();var gg=function(t){return{ids:function(){var e=Re(t)(),n=zt(Xa)(hc(m_(kg))({newSeed:kc(e),size:5}));return Ar(F)(tc(Br(Fu)(1))(t))(),n},makeElement:$k(!1),attributeParent:Ok,makeRoot:Nk,makeText:Mk(!1)(Yt(void 0)),makePursx:Rk(!1)(Yt(void 0)),setProp:wk(!1),setCb:Pk(!1),setText:Ik,sendToTop:Wk,deleteFromCache:Uk,giveNewParent:Lk,disconnectElement:Bk}};var B$=function(t){return t};var K=function(t){return{pursxToElement:function(r){return function(e){return function(n){return{cache:Io,element:function(a){return function(u){return M(h(t))}}}}}}}},rD=function(t){return t.pursxToElement},on=function(){return function(t){return function(r){return function(e){return function(n){return{pursxToElement:function(a){return function(u){return function(i){var o=rD(t)(a)(d.value)(i);return{cache:_i(Po(r)(d.value))(!0)(o.cache),element:function(m){return function(s){return N(I(n.MonadST5().Monad0().Applicative0()))(_(g)(Ka(Un)(Up)(function(l){if(l.value instanceof Kl)return s.setProp({id:Po(r)(d.value)+a,key:l.key,value:l.value.value0});if(l.value instanceof Yl)return s.setCb({id:Po(r)(d.value)+a,key:l.key,value:l.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4474, column 38 - line 4484, column 24): "+[l.value.constructor.name])}))(ci(e)()(d.value)(i)))(o.element(m)(s))}}}}}}}}}}}};var L=B$,pr=function(t){return function(r){return function(){return function(){return function(e){return function(n){return function(a){return function(u){return function(i){var o=function(m){return function(s){return Bt(function(l){return H(e.MonadST5().Monad0().Bind1())(s.ids)(function(v){return H(e.MonadST5().Monad0().Bind1())(s.ids)(function(D){return ae(jr)(e.MonadST5().Monad0().Bind1())(m.raiseId(v))(function(){var c=rD(n)(D)(d.value)(i);return _(e.MonadST5().Monad0().Bind1().Apply0().Functor0())(X(e.MonadST5().Monad0().Bind1().Apply0())(l(s.deleteFromCache({id:v}))))(Ot(N(I(e.MonadST5().Monad0().Applicative0()))($t(e.MonadST5().Monad0().Applicative0())(s.makePursx({id:v,parent:m.parent,cache:c.cache,pxScope:D,scope:m.scope,html:Po(t)(u),verb:Po(r)(a)})))(c.element(m)(s)))(l))})})})})}};return new P(o)}}}}}}}}},Mt=function(t){return function(){return function(){return function(r){return function(e){return pr(t)({reflectType:function(){return"~"}})()()(e)(r)(d.value)}}}}};var U$=function(t){return oo(t.MonadST5().Monad0().Applicative0())(t.MonadST5())({doLogic:function(r){return function(e){return function(n){return e.sendToTop({id:n})}}},ids:function(){var r=Ce();return function(e){return function(n){return n.ids}(r(e))}}(),disconnectElement:function(r){return function(e){return r.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:Zt(Pp)})}},toElt:function(r){return r}})},B=function(){return function(t){return function(r){return function(e){return function(n){return{pursxToElement:function(a){return function(u){return function(i){var o=ci(e)()(d.value)(i),m=rD(t)(a)(d.value)(i);return{cache:_i(Po(r)(d.value))(!1)(m.cache),element:function(s){return function(l){return N(I(n.MonadST5().Monad0().Applicative0()))(U$(n)({parent:new U(Po(r)(d.value)+a),scope:s.scope,raiseId:function(v){return S(n.MonadST5().Monad0().Applicative0())(void 0)}})(l)(o))(m.element(s)(l))}}}}}}}}}}}};var At=function(){return function(){return{defaults:xt(KA()())}}},W$=function(t){return t.defaults},kt={convertRecordOptions:function(t){return function(r){return function(e){return tt(qp)}}}},Cg=function(t){return t.convertRecordOptions},sa=function(t){return t.convertOptionsWithDefaults},gt=function(){return function(t){return{convertOptions:function(r){return function(e){return Ck(Cg(t)(r)(d.value)(e))}}}}},q$=function(t){return t.convertOptions},Ct=function(t){return function(r){return{convertOptionsWithDefaults:function(e){return function(n){var a=W$(r)(n),u=q$(t)(e);return function(i){return a(u(i))}}}}}},H$=function(t){return t.convertOption},J=function(t){return function(r){return function(){return function(){return function(){return function(e){return{convertRecordOptions:function(n){return function(a){return function(u){return Mu(Wp)(Sv()()(e)(d.value)(H$(r)(n)(d.value)(ci(e)()(d.value)(u))))(Cg(t)(n)(d.value)(u))}}}}}}}}}};var eD=function(){var t=yp(jm);return function(r){return Ap(t(r))}}();var Wz=typeof Array.from=="function",qz=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",Hz=typeof String.prototype.fromCodePoint=="function",zz=typeof String.prototype.codePointAt=="function";var mi={proof:function(t){return t},Coercible0:function(){}},aD=function(t){return t.proof};var ku=void 0;var cs=function(t){return t.toInt},Sg=function(t){return function(r){return cs(t)(ku)}};var Za={toInt:function(t){return 8}},xg={Nat0:function(){return Za}},Bo={toInt:function(t){return 7}},Fg={Nat0:function(){return Bo}},Uo={toInt:function(t){return 6}},Og={Nat0:function(){return Uo}},xa={toInt:function(t){return 5}},ls={Nat0:function(){return xa}},qn={toInt:function(t){return 4}},aa={Nat0:function(){return qn}},Hn={toInt:function(t){return 3}},gu={Nat0:function(){return Hn}},zn={toInt:function(t){return 2}},Cu={Nat0:function(){return zn}},Gn={toInt:function(t){return 1}},Eu={Nat0:function(){return Gn}},$e={toInt:function(t){return 0}};var Mr=function(t){return function(){return function(r){return function(){return function(e){return{Nat0:r.Nat1,Pos1:function(){return t}}}}}}};var lo={Nat0:function(){return Bo},Nat1:function(){return Za}};var _o={Nat0:function(){return Uo},Nat1:function(){return Za}};var po={Nat0:function(){return xa},Nat1:function(){return Za}};var so={Nat0:function(){return qn},Nat1:function(){return Za}};var ma={Nat0:function(){return qn},Nat1:function(){return xa}};var mo={Nat0:function(){return Hn},Nat1:function(){return Za}};var va={Nat0:function(){return Hn},Nat1:function(){return xa}};var vo={Nat0:function(){return zn},Nat1:function(){return Za}};var Da={Nat0:function(){return zn},Nat1:function(){return xa}};var Do={Nat0:function(){return Gn},Nat1:function(){return Za}};var da={Nat0:function(){return Gn},Nat1:function(){return xa}};var bo={Nat0:function(){return $e},Nat1:function(){return Za}};var ba={Nat0:function(){return $e},Nat1:function(){return xa}};var $g={Nat0:function(){return $e},Nat1:function(){return Za}};var uD={Nat0:function(){return $e},Nat1:function(){return Bo}};var oD={Nat0:function(){return $e},Nat1:function(){return Uo}};var D_={Nat0:function(){return $e},Nat1:function(){return xa}};var Ha={Nat0:function(){return $e},Nat1:function(){return qn}};var vn={Nat0:function(){return $e},Nat1:function(){return Hn}};var Dn={Nat0:function(){return $e},Nat1:function(){return zn}};var dn={Nat0:function(){return $e},Nat1:function(){return Gn}},hu={Nat0:function(){return $e},Nat1:function(){return $e}};var Mg=ri;var _s=function(t){return t};var d_=function(t){return function(){return function(r){return function(e){return r[cs(t)(e)]}}}};var ps=function(t){return function(r){var e=Sg(t)(d.value),n=function(){return e===0?[]:pn(0)(e-1|0)}();return _(Or)(r)(n)}};var Wu=[];var wr=function(t){return function(r){return function(e){return Ui(r)(e)}}};var bn={first:function(t){return function(r){return new at(t(r.value0),r.value1)}},second:_(ti),Profunctor0:function(){return Un}},Vn=function(t){return t.second},ss=function(t){return t.first};var AM=function(t){return function(r){return function(e){return function(n){return wo(e)(t)(r)(n)}}}};var Rg=function(){return function(){return function(t){return AM(Ln())(Ln())(t)}}};var Ng=function(){return function(){return function(t){return Rg()()(t)}}};var CM=function(t){return function(r){return function(e){return wo(r.Profunctor0())(t)(function(n){return n.value1(n.value0)})(ss(r)(e))}}},Lg=function(t){return function(r){return function(e){return CM(function(n){return new at(t(n),function(a){return r(n)(a)})})(e)}}};var Bg=function(t){return function(){return function(){return function(r){return function(e){return Lg(ci(t)()(r))(xt(QA(t)()()(r)))(e)}}}}};var Ug=function(t){return t};var FM=JSON.parse;var OM=JSON.stringify;var ms=function(t){return t};var vs=function(t){return t};var Ds=function(t){return function(r){return t(r)}},b_=function(t){return{map:function(r){return Ds(_(t)(_(yf)(r)))}}};var cD=function(t){return{Applicative0:function(){return y_(t)},Bind1:function(){return lD(t)}}},lD=function(t){return{bind:function(r){return function(e){return H(t.Bind1())(r)(Ra(function(){var n=S(t.Applicative0());return function(a){return n(jt.create(a))}}())(function(n){var a=e(n);return a}))}},Apply0:function(){return Hg(t)}}},Hg=function(t){return{apply:ju(cD(t)),Functor0:function(){return b_(t.Bind1().Apply0().Functor0())}}},y_=function(t){return{pure:function(){var r=S(t.Applicative0());return function(e){return ms(r(Xt.create(e)))}}(),Apply0:function(){return Hg(t)}}};var zg=function(t){return{throwError:function(){var r=S(t.Applicative0());return function(e){return ms(r(jt.create(e)))}}(),Monad0:function(){return cD(t)}}};var _D=function(t){return function(r){return{alt:function(e){return function(n){return H(r.Bind1())(e)(function(a){if(a instanceof Xt)return S(r.Applicative0())(new Xt(a.value0));if(a instanceof jt)return H(r.Bind1())(n)(function(u){if(u instanceof Xt)return S(r.Applicative0())(new Xt(u.value0));if(u instanceof jt)return S(r.Applicative0())(new jt(bt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return b_(r.Bind1().Apply0().Functor0())}}}};var pD=function(){var t=Ce();return function(r){return t(vs(r))}}();function Vg(t,r,e){return t==null?r:e(t)}var Ze=function(t){return Vg(t,V.value,U.create)};function A_(t){return Object.prototype.toString.call(t).slice(8,-1)}var Yg=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var dD=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Zg=lt;var bD=function(t){var r=fo(zg(t));return function(e){return r(eD(e))}};var yD=function(t){return function(r){return function(e){if(A_(e)===r)return S(y_(t))(Zg(e));if(ne)return bD(t)(new dD(r,A_(e)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[r.constructor.name,e.constructor.name])}}};var AD=function(t){return yD(t)("String")};var ys=function(){function t(){}return t.value=new t,t}(),As=function(){function t(){}return t.value=new t,t}(),eC=function(){function t(){}return t.value=new t,t}(),nC=function(){function t(){}return t.value=new t,t}(),gD=function(){function t(){}return t.value=new t,t}(),aC=function(){function t(){}return t.value=new t,t}(),uC=function(){function t(){}return t.value=new t,t}();var oC=function(t){return t},iC=function(t){return t};var fC=function(t){return t};var cC=function(t){return t};var lC=function(t){return t};var _C=function(t){return t},pC=function(t){return t},sC=function(t){return t},mC=function(t){return t},vC=function(t){return t};var CD=function(){function t(){}return t.value=new t,t}(),DC=function(){function t(){}return t.value=new t,t}(),dC=function(){function t(){}return t.value=new t,t}(),ED=function(){function t(){}return t.value=new t,t}(),bC=function(){function t(){}return t.value=new t,t}();var ks=function(t){return t};var Fc=function(t){return t};var pw=function(t){return t},k_=function(t){return t};var Rf={toAudioOnOff:tt(rt)};var Nf=function(t){return t.toAudioParameter},yC=function(t){return t.toAudioOnOff},AC=function(){return sc.create}(),kC=function(){return mc.value}();var gs=function(){return Ug(function(){var t=Ng()()(Un),r=Bg({reflectSymbol:function(){return"o"}})()()(d.value)(bn);return function(e){return t(r(e))}}())},gC=lt;var sw=function(){var t=ie()({reflectSymbol:function(){return"unit"}})(d.value);return function(r){return k_(t(r))}}();var mw=function(t){return function(r){return{toAudioParameter:function(e){return sw(e)}}}},CC=function(t){return function(r){return{toAudioParameter:function(){var e=Nf(mw(t)(r));return function(n){return e(pw(function(a){return{u:a}}(n)))}}()}}},EC=function(){return ie()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),hC=function(){var t=ie()({reflectSymbol:function(){return"sudden"}})(d.value);return function(r){return k_(t(r))}}();var TC={toAudioParameter:hC},Cs={toAudioParameter:function(t){return hC({n:t})}},hD=function(){return ie()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var TD=function(){return ie()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),g_={x:TD,o:0},_t=function(){return Y(T(k))(nn()(ie()({reflectSymbol:function(){return"onOff"}})(d.value)(g_)))};var SC=function(){return ie()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var vw=function(){var t=ie()({reflectSymbol:function(){return"numeric"}})(d.value);return function(r){return k_(t(r))}}();var Be={toAudioParameter:vw};var Wo=function(){return ie()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var xC=function(){return ie()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),Dw=function(){var t=ie()({reflectSymbol:function(){return"envelope"}})(d.value);return function(r){return k_(t(r))}}();var Mn={toAudioParameter:Dw},dw=function(){var t=ie()({reflectSymbol:function(){return"cancel"}})(d.value);return function(r){return k_(t(r))}}();var FC={toAudioParameter:dw};var bw=function(){function t(){}return t.value=new t,t}(),yw=function(){function t(){}return t.value=new t,t}(),Aw=function(){function t(){}return t.value=new t,t}(),kw=function(){function t(){}return t.value=new t,t}(),gw=function(){function t(){}return t.value=new t,t}(),Cw=function(){function t(){}return t.value=new t,t}(),Ew=function(){function t(){}return t.value=new t,t}(),hw=function(){function t(){}return t.value=new t,t}(),Tw=function(){function t(){}return t.value=new t,t}(),Sw=function(){function t(){}return t.value=new t,t}(),xw=function(){function t(){}return t.value=new t,t}(),Fw=function(){function t(){}return t.value=new t,t}(),Ow=function(){function t(){}return t.value=new t,t}(),$w=function(){function t(){}return t.value=new t,t}(),vi=function(t){return{toPeriodicOscSpec:function(r){return ie()({reflectSymbol:function(){return"realImg"}})(d.value)({real:_s(r.value0),img:_s(r.value1)})}}};var Es={toInitializeTriangleOsc:function(t){return vC(function(r){return{frequency:r}}(t))}};var OC={toInitializeStereoPanner:function(t){return mC(function(r){return{pan:r}}(t))}};var Oc={toInitializeSquareOsc:function(t){return sC(function(r){return{frequency:r}}(t))}};var ef={toInitializeSinOsc:function(t){return pC(function(r){return{frequency:r}}(t))}};var $C={toInitializeSawtoothOsc:function(t){return _C(function(r){return{frequency:r}}(t))}};var SD={toInitializeRecorder:function(t){return oC(function(r){return{cb:r}}(t))}};var C_={toInitializeMicrophone:function(t){return iC(function(r){return{microphone:r}}(t))}};var MC=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:aD(mi)(Ln()(e.value0)),feedback:aD(mi)(Ln()(e.value1))}}}}}}};var ft={toInitializeGain:function(t){return lC(function(r){return{gain:r}}(t))}};var wC={toInitializeConvolver:function(t){return fC(function(r){return{buffer:r}}(t))}},hs={toInitializeConstant:function(t){return cC(function(r){return{offset:r}}(t))}};var Mw={convertOption:function(t){return function(r){return tt(rt)}}},E_={convertOption:function(t){return function(r){return tt(rt)}}},PC={convertOption:function(t){return function(r){return tt(rt)}}},IC={convertOption:function(t){return function(r){return U.create}}},RC={convertOption:function(t){return function(r){return tt(rt)}}},Di={convertOption:function(t){return function(r){return tt(rt)}}},$c={convertOption:function(t){return function(r){return tt(rt)}}},Mc={convertOption:function(t){return function(r){return tt(rt)}}},wc={convertOption:function(t){return function(r){return tt(rt)}}},Pc={convertOption:function(t){return function(r){return tt(rt)}}},Ic={convertOption:function(t){return function(r){return tt(rt)}}},NC={convertOption:function(t){return function(r){return tt(rt)}}},LC={convertOption:function(t){return function(r){return tt(rt)}}},BC={convertOption:function(t){return function(r){return tt(rt)}}},xD={convertOption:function(t){return function(r){return tt(rt)}}},Lf={convertOption:function(t){return function(r){return tt(rt)}}},h_={convertOption:function(t){return function(r){return tt(rt)}}},T_={convertOption:function(t){return function(r){return tt(rt)}}};var Rc={convertOption:function(t){return function(r){return tt(rt)}}},UC={convertOption:function(t){return function(r){return tt(rt)}}},WC={convertOption:function(t){return function(r){return tt(rt)}}},qC={convertOption:function(t){return function(r){return tt(rt)}}},FD={convertOption:function(t){return function(r){return tt(rt)}}};var HC={convertOption:function(t){return function(r){return tt(rt)}}},OD={convertOption:function(t){return function(r){return tt(rt)}}},An={convertOption:function(t){return function(r){return tt(rt)}}},fn={convertOption:function(t){return function(r){return tt(rt)}}},$D={convertOption:function(t){return function(r){return tt(rt)}}},Ts={convertOption:function(t){return function(r){return tt(rt)}}},ww=function(t){return t.toPeriodicOscSpec},di=function(t){return{convertOption:function(r){return function(e){return ww(t)}}}},MD=function(t){return t.toInitializeWaveShaper},zC=function(t){return t.toInitializeTriangleOsc},GC=function(t){return t.toInitializeStereoPanner},VC=function(t){return t.toInitializeSquareOsc},JC=function(t){return t.toInitializeSinOsc},jC=function(t){return t.toInitializeSawtoothOsc},XC=function(t){return t.toInitializeRecorder},wD=function(t){return t.toInitializePlayBuf},QC=function(t){return t.toInitializePeriodicOsc},KC=function(t){return t.toInitializePeaking},YC=function(t){return t.toInitializeNotch},ZC=function(t){return t.toInitializeMicrophone},tE=function(t){return t.toInitializeLowshelf},PD=function(t){return t.toInitializeLowpass},ID=function(t){return t.toInitializeLoopBuf},rE=function(t){return t.toInitializeIIRFilter},eE=function(t){return t.toInitializeHighshelf},RD=function(t){return t.toInitializeHighpass},nE=function(t){return t.toInitializeGain},aE=function(t){return t.toInitializeDynamicsCompressor},ND=function(t){return t.toInitializeDelay},uE=function(t){return t.toInitializeConvolver},oE=function(t){return t.toInitializeConstant},LD=function(t){return t.toInitializeBandpass},BD=function(t){return t.toInitializeAllpass};var Pw={oversample:EC},Iw=function(t){return{toInitializeWaveShaper:function(r){return sa(t)(bw.value)(Pw)(r)}}},iE={toInitializeWaveShaper:function(){var t=MD(Iw(Ct(gt()(J(kt)(Mw)()()()({reflectSymbol:function(){return"curve"}})))(At()())));return function(r){return t(function(e){return{curve:e}}(r))}}()},Rw=function(){return{bufferOffset:0,playbackRate:1,duration:V.value}}(),S_=function(t){return{toInitializePlayBuf:function(r){return sa(t)(yw.value)(Rw)(r)}}},za={toInitializePlayBuf:function(){var t=wD(S_(Ct(gt()(J(kt)(E_)()()()({reflectSymbol:function(){return"buffer"}})))(At()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},Nw={},bi=function(t){return{toInitializePeriodicOsc:function(r){return sa(t)(Aw.value)(Nw)(r)}}},Lw={q:1,gain:0},Nc=function(t){return{toInitializePeaking:function(r){return sa(t)(kw.value)(Lw)(r)}}};var Bw={q:1},Lc=function(t){return{toInitializeNotch:function(r){return sa(t)(gw.value)(Bw)(r)}}};var Uw={gain:0},fE=function(t){return{toInitializeLowshelf:function(r){return sa(t)(Cw.value)(Uw)(r)}}};var Ww={q:1},UD=function(t){return{toInitializeLowpass:function(r){return sa(t)(Ew.value)(Ww)(r)}}},Ss={toInitializeLowpass:function(){var t=PD(UD(Ct(gt()(J(kt)(xD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},qw=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:V.value}}(),Bf=function(t){return{toInitializeLoopBuf:function(r){return sa(t)(hw.value)(qw)(r)}}},Cr={toInitializeLoopBuf:function(){var t=ID(Bf(Ct(gt()(J(kt)(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},Hw={gain:0},cE=function(t){return{toInitializeHighshelf:function(r){return sa(t)(Tw.value)(Hw)(r)}}};var zw={q:1},WD=function(t){return{toInitializeHighpass:function(r){return sa(t)(Sw.value)(zw)(r)}}},tu={toInitializeHighpass:function(){var t=RD(WD(Ct(gt()(J(kt)(FD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},Gw=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),lE=function(t){return{toInitializeDynamicsCompressor:function(r){return sa(t)(xw.value)(Gw)(r)}}},Vw={maxDelayTime:1},qD=function(t){return{toInitializeDelay:function(r){return sa(t)(Fw.value)(Vw)(r)}}},tn={toInitializeDelay:function(){var t=ND(qD(Ct(gt()(J(kt)(OD)()()()({reflectSymbol:function(){return"delayTime"}})))(At()())));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},Jw={q:1},cn=function(t){return{toInitializeBandpass:function(r){return sa(t)(Ow.value)(Jw)(r)}}},HD={toInitializeBandpass:function(){var t=LD(cn(Ct(gt()(J(kt)(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},jw={q:1},xs=function(t){return{toInitializeAllpass:function(r){return sa(t)($w.value)(jw)(r)}}},zD={toInitializeAllpass:function(){var t=BD(xs(Ct(gt()(J(kt)(Ts)()()()({reflectSymbol:function(){return"frequency"}})))(At()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var qo=function(){function t(){this.head=null,this.last=null,this.size=0}function r(l,v){this.queue=l,this.value=v,this.next=null,this.prev=null}function e(l){this.draining=!1,this.error=null,this.value=l,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(l){try{l()}catch(v){setTimeout(function(){throw v},0)}}function u(l,v){var D=new r(l,v);switch(l.size){case 0:l.head=D;break;case 1:D.prev=l.head,l.head.next=D,l.last=D;break;default:D.prev=l.last,l.last.next=D,l.last=D}return l.size++,D}function i(l){var v;switch(l.size){case 0:return null;case 1:v=l.head,l.head=null;break;case 2:v=l.last,l.head.next=null,l.last=null;break;default:v=l.last,l.last=v.prev,l.last.next=null}return v.prev=null,v.queue=null,l.size--,v.value}function o(l){var v;switch(l.size){case 0:return null;case 1:v=l.head,l.head=null;break;case 2:v=l.head,l.last.prev=null,l.head=l.last,l.last=null;break;default:v=l.head,l.head=v.next,l.head.prev=null}return v.next=null,v.queue=null,l.size--,v.value}function m(l){if(l.queue!==null){if(l.queue.last===l){i(l.queue);return}if(l.queue.head===l){o(l.queue);return}l.prev&&(l.prev.next=l.next),l.next&&(l.next.prev=l.prev),l.queue.size--,l.queue=null,l.value=null,l.next=null,l.prev=null}}function s(l,v){if(!v.draining){var D=v.puts,c=v.takes,C=v.reads,ut,mt,vr,nr,Zr;for(v.draining=!0;;){if(ut=null,mt=null,vr=null,nr=v.value,Zr=C.size,v.error!==null){for(nr=l.left(v.error);ut=o(D);)a(ut.cb(nr));for(;mt=o(C);)a(mt(nr));for(;vr=o(c);)a(vr(nr));break}if(nr===n&&(ut=o(D))&&(v.value=nr=ut.value),nr!==n){for(vr=o(c);Zr--&&(mt=o(C));)a(mt(l.right(nr)));vr!==null&&(v.value=n,a(vr(l.right(nr))))}if(ut!==null&&a(ut.cb(l.right(void 0))),v.value===n&&D.size===0||v.value!==n&&c.size===0)break}v.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=i,e.takeHead=o,e.deleteCell=m,e.drainVar=s,e}();function x_(){return new qo(qo.EMPTY)}function _E(t,r,e){return function(){var n=qo.putLast(r.takes,e);return qo.drainVar(t,r),function(){qo.deleteCell(n)}}}function pE(t,r,e){return function(){return e.value===qo.EMPTY&&e.error===null?(e.value=r,qo.drainVar(t,e),!0):!1}}function sE(t,r){return function(){var e=r.value;return e===qo.EMPTY?t.nothing:(r.value=qo.EMPTY,qo.drainVar(t,r),t.just(e))}}var Yw=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Zw=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),tP=function(){function t(){}return t.value=new t,t}();var GD=function(){return{left:jt.create,right:Xt.create,nothing:V.value,just:U.create,killed:Yw.create,filled:Zw.create,empty:tP.value}}();var mE=function(t){return function(r){return _E(GD,t,r)}},Fs=function(t){return function(r){return pE(GD,t,r)}};var vE=function(t){return sE(GD,t)};var rP=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var eP=function(){function t(){}return t.value=new t,t}();var Os={convertOption:function(t){return function(r){return tt(rt)}}},$s={convertOption:function(t){return function(r){return tt(rt)}}};var nP=function(t){return t.toInitializeAnalyser},ru=mk(k)({doLogic:Df,ids:function(){var t=Ce();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:Ln(),connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var aP=function(){return{cb:function(t){return S(f)(S(f)(void 0))},fftSize:gD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:ED.value,channelInterpretation:CD.value}}(),Ms=function(t){return{toInitializeAnalyser:function(r){return sa(t)(eP.value)(aP)(r)}}};var uP=function(t){return function(r){var e=ZC(t)(r),n=function(a){return function(u){return Bt(function(i){return function(){var m=u.ids();return a.raiseId(m)(),_(F)(function(s){return X(nt)(i(u.deleteFromCache({id:m})))(s)})(xt(Ot)(i)($t(f)(u.makeMicrophone({id:m,parent:a.parent,scope:a.scope,microphone:e.microphone}))))()}})}};return new P(n)}},F_=function(t){return uP(t)};var Jn=oo(f)(k)({doLogic:Df,ids:function(){var t=Ce();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),oP=function(t){return function(r){return function(e){return function(n){var a=nP(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeAnalyser({id:l,parent:i.parent,scope:i.scope,cb:a.cb,fftSize:Pv(2)(function(){if(a.fftSize instanceof ys)return 7;if(a.fftSize instanceof As)return 8;if(a.fftSize instanceof eC)return 9;if(a.fftSize instanceof nC)return 10;if(a.fftSize instanceof gD)return 11;if(a.fftSize instanceof aC)return 12;if(a.fftSize instanceof uC)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 189, column 21 - line 196, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof bC)return"explicit";if(a.channelCountMode instanceof ED)return"max";if(a.channelCountMode instanceof dC)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 202, column 35 - line 205, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof CD)return"speakers";if(a.channelInterpretation instanceof DC)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 206, column 40 - line 208, column 41): "+[a.channelInterpretation.constructor.name])}()})))(N(I(f))(_(g)(function(v){return qe()()()({cb:function(D){return o.setAnalyserNodeCb({id:l,cb:D})}})(v)})(e))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},ws=function(t){return function(r){return oP(t)(r)(M(h(f)))}},dE=function(t){return function(r){return function(e){var n=uE(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(Jn({parent:new U(s),scope:u.scope,raiseId:wt(Qe(ge(Ie)))})(i)(W(e)))))()}})}};return new P(a)}}},iP=function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){var u=rE(t)(n)(r)(e),i=function(o){return function(m){return Bt(function(s){return function(){var v=m.ids();return o.raiseId(v)(),_(F)(function(D){return X(nt)(s(m.deleteFromCache({id:v})))(D)})(xt(Ot)(s)(N(I(f))($t(f)(m.makeIIRFilter({id:v,parent:o.parent,scope:o.scope,feedforward:Tf()(u.feedforward),feedback:Tf()(u.feedback)})))(Jn({parent:new U(v),scope:o.scope,raiseId:wt(Qe(ge(Ie)))})(m)(W(a)))))()}})}};return new P(i)}}}}}}},bE=function(){return function(){return function(t){return iP()()(t)(d.value)(d.value)}}},VD=function(t){return function(r){return function(e){var n=XC(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(Jn({parent:new U(s),scope:u.scope,raiseId:wt(Qe(ge(Ie)))})(i)(e))))()}})}};return new P(a)}}},fP=function(t){return function(r){return Bt(function(e){return function(){var a=r.ids();return e(r.makeSpeaker({id:a}))(),Ot(Jn({parent:new U(a),scope:new Ki("toplevel"),raiseId:wt(Qe(ge(Ie)))})(r)(W(t)))(e)()}})}},Uf=fP,Pt=function(t){return function(r){return function(e){return He(t)(r)(M(h(f)))(e)}}},He=function(t){return function(r){return function(e){return function(n){var a=nE(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeGain({id:l,parent:i.parent,scope:i.scope,gain:a.gain})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({gain:yE(591)(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},yE=rP("tmpResolveAU","Ocarina.Control",function(){var t=function(){var i=ie()({reflectSymbol:function(){return"unit"}})(d.value);return function(o){return Fc(i(o))}}(),r=function(){var i=ie()({reflectSymbol:function(){return"sudden"}})(d.value);return function(o){return Fc(i(o))}}(),e=function(){var i=ie()({reflectSymbol:function(){return"numeric"}})(d.value);return function(o){return Fc(i(o))}}(),n=function(){var i=ie()({reflectSymbol:function(){return"envelope"}})(d.value);return function(o){return Fc(i(o))}}(),a=function(){var i=ie()({reflectSymbol:function(){return"cancel"}})(d.value);return function(o){return Fc(i(o))}}(),u=function(i){return function(o){return function(m){return function(s){return qe()()()({numeric:function(){var l=$t(f);return function(v){return l(m(e(v)))}}(),envelope:function(){var l=$t(f);return function(v){return l(m(n(v)))}}(),cancel:function(){var l=$t(f);return function(v){return l(m(a(v)))}}(),sudden:function(){var l=$t(f);return function(v){return l(m(r(v)))}}(),unit:function(l){var v=Pt(ft)(1)([l.u]);return Bt(function(D){return function(){var C=x_();return Ot(N(I(f))(Jn({parent:V.value,scope:i,raiseId:function(ut){return Ar(F)(Fs(ut)(C))}})(o)(v))(Bt(function(ut){return function(){return Ar(F)(mE(C)(function(vr){if(vr instanceof jt)return Of(vr.value0);if(vr instanceof Xt)return ut(m(t({i:vr.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1674, column 39 - line 1677, column 66): "+[vr.constructor.name])}))(),S(f)(void 0)}})))(D)()}})}})(s)}}}};return u}),ce=yE(1653),cP=function(t){return function(r){return function(e){var n=ID(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(Oe(T(k))(_(g)(function(l){return qe()()()({buffer:function(v){return $t(f)(i.setBuffer({id:s,buffer:v}))},playbackRate:ce(u.scope)(i)(function(v){return i.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),loopStart:function(v){return $t(f)(i.setLoopStart({id:s,loopStart:v}))},loopEnd:function(v){return $t(f)(i.setLoopEnd({id:s,loopEnd:v}))},onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(e)))))()}})}};return new P(a)}}},lr=function(t){return cP(t)};var lP=function(t){return function(r){return function(e){var n=QC(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))(Oe(T(k))(_(g)(function(l){return qe()()()({frequency:ce(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))},spec:function(v){return $t(f)(i.setPeriodicOsc({id:s,spec:v}))}})(l)})(e)))))()}})}};return new P(a)}}},yi=function(t){return lP(t)};var _P=function(t){return function(r){return function(e){var n=wD(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(Oe(T(k))(_(g)(function(l){return qe()()()({buffer:function(v){return $t(f)(i.setBuffer({id:s,buffer:v}))},playbackRate:ce(u.scope)(i)(function(v){return i.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),bufferOffset:function(v){return $t(f)(i.setBufferOffset({id:s,bufferOffset:v}))},onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))},duration:function(v){return $t(f)(i.setDuration({id:s,duration:v}))}})(l)})(e)))))()}})}};return new P(a)}}},jn=function(t){return _P(t)};var pP=function(t){return function(r){return function(e){var n=jC(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(Oe(T(k))(_(g)(function(l){return qe()()()({frequency:ce(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(e)))))()}})}};return new P(a)}}},AE=function(t){return pP(t)};var sP=function(t){return function(r){return function(e){var n=JC(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(Oe(T(k))(_(g)(function(l){return qe()()()({frequency:ce(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(e)))))()}})}};return new P(a)}}},Wf=function(t){return sP(t)},kE=function(t){return function(r){return Wf(t)(r)(M(h(f)))}},mP=function(t){return function(r){return function(e){var n=VC(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(Oe(T(k))(_(g)(function(l){return qe()()()({frequency:ce(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(e)))))()}})}};return new P(a)}}},O_=function(t){return mP(t)},gE=function(t){return function(r){return O_(t)(r)(M(h(f)))}},vP=function(t){return function(r){return function(e){var n=zC(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(Oe(T(k))(_(g)(function(l){return qe()()()({frequency:ce(u.scope)(i)(function(v){return i.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(e)))))()}})}};return new P(a)}}},Ps=function(t){return vP(t)};var DP=function(t){return function(r){return function(e){return function(n){var a=BD(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeAllpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:ce(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},$_=function(t){return function(r){return function(e){return DP(t)(r)(M(h(f)))(e)}}},JD=function(t){return function(r){return function(e){return function(n){var a=LD(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeBandpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:ce(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},kn=function(t){return function(r){return function(e){return JD(t)(r)(M(h(f)))(e)}}},M_=function(t){return function(r){return function(e){return function(n){var a=ND(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeDelay({id:l,parent:i.parent,scope:i.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({delayTime:ce(i.scope)(o)(function(D){return o.setDelay(function(c){return{id:l,delayTime:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},yo=function(t){return function(r){return function(e){return M_(t)(r)(M(h(f)))(e)}}},dP=function(t){return function(r){return function(e){return function(n){var a=aE(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeDynamicsCompressor({id:l,parent:i.parent,scope:i.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({threshold:ce(i.scope)(o)(function(D){return o.setThreshold(function(c){return{id:l,threshold:c}}(D))}),ratio:ce(i.scope)(o)(function(D){return o.setRatio(function(c){return{id:l,ratio:c}}(D))}),knee:ce(i.scope)(o)(function(D){return o.setKnee(function(c){return{id:l,knee:c}}(D))}),attack:ce(i.scope)(o)(function(D){return o.setAttack(function(c){return{id:l,attack:c}}(D))}),release:ce(i.scope)(o)(function(D){return o.setRelease(function(c){return{id:l,release:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},CE=function(t){return function(r){return dP(t)(r)(M(h(f)))}},bP=function(){return function(t){return function(r){return hv()(k)({doLogic:Df,ids:function(){var e=Ce();return function(n){return function(a){return a.ids}(e(n))}}(),disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:Ln(),fromEltO2:Ln(),toElt:Ln(),wrapElt:function(e){return Pt(ft)(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var e=Ce();return function(n){return function(a){return a.deleteFromCache}(e(n))}}()})(t)(Ka(Un)(_(jl)(function(e){return e(void 0)}))(Ln()(r)))}}},Fa=function(t){return function(r){return bP()(ek(t))(Ka(Un)(nk()()()()()({reflectType:function(){return 0}})(d.value))(r))}};var jD=function(t){return function(r){return function(e){return function(n){var a=RD(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeHighpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:ce(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},Bc=function(t){return function(r){return function(e){return jD(t)(r)(M(h(f)))(e)}}},yP=function(t){return function(r){return function(e){return function(n){var a=eE(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeHighshelf({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,gain:a.gain})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),gain:ce(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},EE=function(t){return function(r){return function(e){return yP(t)(r)(M(h(f)))(e)}}},hE=function(t){return function(r){return function(e){return function(n){var a=PD(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeLowpass({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:ce(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},Uc=function(t){return function(r){return function(e){return hE(t)(r)(M(h(f)))(e)}}},AP=function(t){return function(r){return function(e){return function(n){var a=tE(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeLowshelf({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,gain:a.gain})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),gain:ce(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},TE=function(t){return function(r){return function(e){return AP(t)(r)(M(h(f)))(e)}}},kP=function(t){return function(r){return function(e){return function(n){var a=YC(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeNotch({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:ce(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},Wc=function(t){return function(r){return function(e){return kP(t)(r)(M(h(f)))(e)}}},gP=function(t){return function(r){return function(e){return function(n){var a=GC(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makeStereoPanner({id:l,parent:i.parent,scope:i.scope,pan:a.pan})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({pan:ce(i.scope)(o)(function(D){return o.setPan(function(c){return{id:l,pan:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},SE=function(t){return function(r){return gP(t)(r)(M(h(f)))}},CP=function(t){return function(r){return function(e){return function(n){var a=KC(t)(r),u=function(i){return function(o){return Bt(function(m){return function(){var l=o.ids();return i.raiseId(l)(),_(F)(function(v){return X(nt)(m(o.deleteFromCache({id:l})))(v)})(xt(Ot)(m)(N(I(f))($t(f)(o.makePeaking({id:l,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(N(I(f))(Oe(T(k))(_(g)(function(v){return qe()()()({frequency:ce(i.scope)(o)(function(D){return o.setFrequency(function(c){return{id:l,frequency:c}}(D))}),q:ce(i.scope)(o)(function(D){return o.setQ(function(c){return{id:l,q:c}}(D))}),gain:ce(i.scope)(o)(function(D){return o.setGain(function(c){return{id:l,gain:c}}(D))})})(v)})(e)))(Jn({parent:new U(l),scope:i.scope,raiseId:wt(Qe(ge(Ie)))})(o)(W(n))))))()}})}};return new P(u)}}}},qc=function(t){return function(r){return function(e){return CP(t)(r)(M(h(f)))(e)}}},xE=function(t){return function(r){return function(e){var n=MD(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(Jn({parent:new U(s),scope:u.scope,raiseId:wt(Qe(ge(Ie)))})(i)(W(e)))))()}})}};return new P(a)}}},EP=function(t){return function(r){return function(e){var n=oE(t)(r),a=function(u){return function(i){return Bt(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(F)(function(l){return X(nt)(o(i.deleteFromCache({id:s})))(l)})(xt(Ot)(o)(N(I(f))($t(f)(i.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))(Oe(T(k))(_(g)(function(l){return qe()()()({offset:ce(u.scope)(i)(function(v){return i.setOffset(function(D){return{id:s,offset:D}}(v))}),onOff:function(v){return $t(f)(i.setOnOff({id:s,onOff:v}))}})(l)})(e)))))()}})}};return new P(a)}}},Is=function(t){return EP(t)};function XD(){window.scrollTo(0,0)}var Ao=function(t){return t.sequential},wn=function(t){return t.parallel};var gn=function(t){return function(r){return function(e){return new P(j(t)("button")(r)(W(e)))}}};var Oa=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",i="Bind",o="Bracket",m="Fork",s="Sequential",l="Map",v="Apply",D="Alt",c="Cons",C="Resume",ut="Release",mt="Finalizer",vr="Finalized",nr="Forked",Zr="Fiber",le="Thunk";function pt(Wt,Xr,Ge,_e){this.tag=Wt,this._1=Xr,this._2=Ge,this._3=_e}function Dr(Wt){var Xr=function(Ge,_e,ir){return new pt(Wt,Ge,_e,ir)};return Xr.tag=Wt,Xr}function Tr(Wt){return new pt(r,void 0)}function ar(Wt){try{Wt()}catch(Xr){setTimeout(function(){throw Xr},0)}}function ze(Wt,Xr,Ge){try{return Xr(Ge())}catch(_e){return Wt(_e)}}function ho(Wt,Xr,Ge){try{return Xr(Ge)()}catch(_e){return Ge(Wt(_e))(),Tr}}var Gu=function(){var Wt=1024,Xr=0,Ge=0,_e=new Array(Wt),ir=!1;function ht(){var ee;for(ir=!0;Xr!==0;)Xr--,ee=_e[Ge],_e[Ge]=void 0,Ge=(Ge+1)%Wt,ee();ir=!1}return{isDraining:function(){return ir},enqueue:function(ee){var Ir,Ue;Xr===Wt&&(Ue=ir,ht(),ir=Ue),_e[(Ge+Xr)%Wt]=ee,Xr++,ir||ht()}}}();function $i(Wt){var Xr={},Ge=0,_e=0;return{register:function(ir){var ht=Ge++;ir.onComplete({rethrow:!0,handler:function(ee){return function(){_e--,delete Xr[ht]}}})(),Xr[ht]=ir,_e++},isEmpty:function(){return _e===0},killAll:function(ir,ht){return function(){if(_e===0)return ht();var ee=0,Ir={};function Ue(ye){Ir[ye]=Xr[ye].kill(ir,function(rn){return function(){delete Ir[ye],ee--,Wt.isLeft(rn)&&Wt.fromLeft(rn)&&setTimeout(function(){throw Wt.fromLeft(rn)},0),ee===0&&ht()}})()}for(var Xn in Xr)Xr.hasOwnProperty(Xn)&&(ee++,Ue(Xn));return Xr={},Ge=0,_e=0,function(ye){return new pt(a,function(){for(var rn in Ir)Ir.hasOwnProperty(rn)&&Ir[rn]()})}}}}}var cu=0,_n=1,Xo=2,Jf=3,jf=4,Me=5,Qo=6;function Xf(Wt,Xr,Ge){var _e=0,ir=cu,ht=Ge,ee=null,Ir=null,Ue=null,Xn=null,ye=null,rn=0,sf=0,xu=null,Mi=!0;function wi(fr){for(var _r,Hr,Jr;;)switch(_r=null,Hr=null,Jr=null,ir){case Xo:ir=_n;try{ht=Ue(ht),Xn===null?Ue=null:(Ue=Xn._1,Xn=Xn._2)}catch(ia){ir=Me,ee=Wt.left(ia),ht=null}break;case Jf:Wt.isLeft(ht)?(ir=Me,ee=ht,ht=null):Ue===null?ir=Me:(ir=Xo,ht=Wt.fromRight(ht));break;case _n:switch(ht.tag){case i:Ue&&(Xn=new pt(c,Ue,Xn)),Ue=ht._2,ir=_n,ht=ht._1;break;case r:Ue===null?(ir=Me,ht=Wt.right(ht._1)):(ir=Xo,ht=ht._1);break;case a:ir=Jf,ht=ze(Wt.left,Wt.right,ht._1);break;case u:ir=jf,ht=ho(Wt.left,ht._1,function(ia){return function(){_e===fr&&(_e++,Gu.enqueue(function(){_e===fr+1&&(ir=Jf,ht=ia,wi(_e))}))}});return;case e:ir=Me,ee=Wt.left(ht._1),ht=null;break;case n:Ue===null?ye=new pt(c,ht,ye,Ir):ye=new pt(c,ht,new pt(c,new pt(C,Ue,Xn),ye,Ir),Ir),Ue=null,Xn=null,ir=_n,ht=ht._1;break;case o:rn++,Ue===null?ye=new pt(c,ht,ye,Ir):ye=new pt(c,ht,new pt(c,new pt(C,Ue,Xn),ye,Ir),Ir),Ue=null,Xn=null,ir=_n,ht=ht._1;break;case m:ir=Jf,_r=Xf(Wt,Xr,ht._2),Xr&&Xr.register(_r),ht._1&&_r.run(),ht=Wt.right(_r);break;case s:ir=_n,ht=AT(Wt,Xr,ht._1);break}break;case Me:if(Ue=null,Xn=null,ye===null)ir=Qo,ht=Ir||ee||ht;else switch(_r=ye._3,Jr=ye._1,ye=ye._2,Jr.tag){case n:Ir&&Ir!==_r&&rn===0?ir=Me:ee&&(ir=_n,ht=Jr._2(Wt.fromLeft(ee)),ee=null);break;case C:Ir&&Ir!==_r&&rn===0||ee?ir=Me:(Ue=Jr._1,Xn=Jr._2,ir=Xo,ht=Wt.fromRight(ht));break;case o:rn--,ee===null&&(Hr=Wt.fromRight(ht),ye=new pt(c,new pt(ut,Jr._2,Hr),ye,_r),(Ir===_r||rn>0)&&(ir=_n,ht=Jr._3(Hr)));break;case ut:ye=new pt(c,new pt(vr,ht,ee),ye,Ir),ir=_n,Ir&&Ir!==_r&&rn===0?ht=Jr._1.killed(Wt.fromLeft(Ir))(Jr._2):ee?ht=Jr._1.failed(Wt.fromLeft(ee))(Jr._2):ht=Jr._1.completed(Wt.fromRight(ht))(Jr._2),ee=null,rn++;break;case mt:rn++,ye=new pt(c,new pt(vr,ht,ee),ye,Ir),ir=_n,ht=Jr._1;break;case vr:rn--,ir=Me,ht=Jr._1,ee=Jr._2;break}break;case Qo:for(var Je in xu)xu.hasOwnProperty(Je)&&(Mi=Mi&&xu[Je].rethrow,ar(xu[Je].handler(ht)));xu=null,Ir&&ee?setTimeout(function(){throw Wt.fromLeft(ee)},0):Wt.isLeft(ht)&&Mi&&setTimeout(function(){if(Mi)throw Wt.fromLeft(ht)},0);return;case cu:ir=_n;break;case jf:return}}function Ve(fr){return function(){if(ir===Qo)return Mi=Mi&&fr.rethrow,fr.handler(ht)(),function(){};var _r=sf++;return xu=xu||{},xu[_r]=fr,function(){xu!==null&&delete xu[_r]}}}function dr(fr,_r){return function(){if(ir===Qo)return _r(Wt.right(void 0))(),function(){};var Hr=Ve({rethrow:!1,handler:function(){return _r(Wt.right(void 0))}})();switch(ir){case cu:Ir=Wt.left(fr),ir=Qo,ht=Ir,wi(_e);break;case jf:Ir===null&&(Ir=Wt.left(fr)),rn===0&&(ir===jf&&(ye=new pt(c,new pt(mt,ht(fr)),ye,Ir)),ir=Me,ht=null,ee=null,wi(++_e));break;default:Ir===null&&(Ir=Wt.left(fr)),rn===0&&(ir=Me,ht=null,ee=null)}return Hr}}function Nr(fr){return function(){var _r=Ve({rethrow:!1,handler:fr})();return ir===cu&&wi(_e),_r}}return{kill:dr,join:Nr,onComplete:Ve,isSuspended:function(){return ir===cu},run:function(){ir===cu&&(Gu.isDraining()?wi(_e):Gu.enqueue(function(){wi(_e)}))}}}function Ko(Wt,Xr,Ge,_e){var ir=0,ht={},ee=0,Ir={},Ue=new Error("[ParAff] Early exit"),Xn=null,ye=t;function rn(Ve,dr,Nr){var fr=dr,_r=null,Hr=null,Jr=0,Je={},ia,cl;t:for(;;)switch(ia=null,fr.tag){case nr:if(fr._3===t&&(ia=ht[fr._1],Je[Jr++]=ia.kill(Ve,function(kT){return function(){Jr--,Jr===0&&Nr(kT)()}})),_r===null)break t;fr=_r._2,Hr===null?_r=null:(_r=Hr._1,Hr=Hr._2);break;case l:fr=fr._2;break;case v:case D:_r&&(Hr=new pt(c,_r,Hr)),_r=fr,fr=fr._1;break}if(Jr===0)Nr(Wt.right(void 0))();else for(cl=0,ia=Jr;cl<ia;cl++)Je[cl]=Je[cl]();return Je}function sf(Ve,dr,Nr){var fr,_r,Hr,Jr,Je,ia;Wt.isLeft(Ve)?(fr=Ve,_r=null):(_r=Ve,fr=null);t:for(;;){if(Hr=null,Jr=null,Je=null,ia=null,Xn!==null)return;if(dr===null){_e(fr||_r)();return}if(dr._3!==t)return;switch(dr.tag){case l:fr===null?(dr._3=Wt.right(dr._1(Wt.fromRight(_r))),_r=dr._3):dr._3=fr;break;case v:if(Hr=dr._1._3,Jr=dr._2._3,fr){if(dr._3=fr,Je=!0,ia=ee++,Ir[ia]=rn(Ue,fr===Hr?dr._2:dr._1,function(){return function(){delete Ir[ia],Je?Je=!1:Nr===null?sf(fr,null,null):sf(fr,Nr._1,Nr._2)}}),Je){Je=!1;return}}else{if(Hr===t||Jr===t)return;_r=Wt.right(Wt.fromRight(Hr)(Wt.fromRight(Jr))),dr._3=_r}break;case D:if(Hr=dr._1._3,Jr=dr._2._3,Hr===t&&Wt.isLeft(Jr)||Jr===t&&Wt.isLeft(Hr))return;if(Hr!==t&&Wt.isLeft(Hr)&&Jr!==t&&Wt.isLeft(Jr))fr=_r===Hr?Jr:Hr,_r=null,dr._3=fr;else if(dr._3=_r,Je=!0,ia=ee++,Ir[ia]=rn(Ue,_r===Hr?dr._2:dr._1,function(){return function(){delete Ir[ia],Je?Je=!1:Nr===null?sf(_r,null,null):sf(_r,Nr._1,Nr._2)}}),Je){Je=!1;return}break}Nr===null?dr=null:(dr=Nr._1,Nr=Nr._2)}}function xu(Ve){return function(dr){return function(){delete ht[Ve._1],Ve._3=dr,sf(dr,Ve._2._1,Ve._2._2)}}}function Mi(){var Ve=_n,dr=Ge,Nr=null,fr=null,_r,Hr;t:for(;;)switch(_r=null,Hr=null,Ve){case _n:switch(dr.tag){case l:Nr&&(fr=new pt(c,Nr,fr)),Nr=new pt(l,dr._1,t,t),dr=dr._2;break;case v:Nr&&(fr=new pt(c,Nr,fr)),Nr=new pt(v,t,dr._2,t),dr=dr._1;break;case D:Nr&&(fr=new pt(c,Nr,fr)),Nr=new pt(D,t,dr._2,t),dr=dr._1;break;default:Hr=ir++,Ve=Me,_r=dr,dr=new pt(nr,Hr,new pt(c,Nr,fr),t),_r=Xf(Wt,Xr,_r),_r.onComplete({rethrow:!1,handler:xu(dr)})(),ht[Hr]=_r,Xr&&Xr.register(_r)}break;case Me:if(Nr===null)break t;Nr._1===t?(Nr._1=dr,Ve=_n,dr=Nr._2,Nr._2=t):(Nr._2=dr,dr=Nr,fr===null?Nr=null:(Nr=fr._1,fr=fr._2))}for(ye=dr,Hr=0;Hr<ir;Hr++)ht[Hr].run()}function wi(Ve,dr){Xn=Wt.left(Ve);var Nr;for(var fr in Ir)if(Ir.hasOwnProperty(fr)){Nr=Ir[fr];for(fr in Nr)Nr.hasOwnProperty(fr)&&Nr[fr]()}Ir=null;var _r=rn(Ve,ye,dr);return function(Hr){return new pt(u,function(Jr){return function(){for(var Je in _r)_r.hasOwnProperty(Je)&&_r[Je]();return Tr}})}}return Mi(),function(Ve){return new pt(u,function(dr){return function(){return wi(Ve,dr)}})}}function AT(Wt,Xr,Ge){return new pt(u,function(_e){return function(){return Ko(Wt,Xr,Ge,_e)}})}return pt.EMPTY=t,pt.Pure=Dr(r),pt.Throw=Dr(e),pt.Catch=Dr(n),pt.Sync=Dr(a),pt.Async=Dr(u),pt.Bind=Dr(i),pt.Bracket=Dr(o),pt.Fork=Dr(m),pt.Seq=Dr(s),pt.ParMap=Dr(l),pt.ParApply=Dr(v),pt.ParAlt=Dr(D),pt.Fiber=Xf,pt.Supervisor=$i,pt.Scheduler=Gu,pt.nonCanceler=Tr,pt}(),FE=Oa.Pure,MP=Oa.Throw;function OE(t){return function(r){return r.tag===Oa.Pure.tag?Oa.Pure(t(r._1)):Oa.Bind(r,function(e){return Oa.Pure(t(e))})}}function $E(t){return function(r){return Oa.Bind(t,r)}}var ME=Oa.Sync;function wE(t){return function(r){return Oa.ParMap(t,r)}}function PE(t){return function(r){return Oa.ParApply(t,r)}}function IE(t){return function(r){return Oa.ParAlt(t,r)}}var Hc=Oa.Async;function RE(t,r){return function(){return Oa.Fiber(t,null,r)}}var wP=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return Oa.Async(function(a){return function(){var u=t(n,a(e()));return function(){return Oa.Sync(function(){return e(r(n,u))})}}})}}(),NE=Oa.Seq;var IP=function(t){return function(r){return function(e){var n=Ao(t),a=ve(t.Applicative1())(r)(function(){var u=wn(t);return function(i){return u(e(i))}}());return function(u){return n(a(u))}}}},LE=function(t){return function(r){return function(e){var n=Ao(t),a=Bn(r)(t.Applicative1())(function(){var u=wn(t);return function(i){return u(e(i))}}());return function(u){return n(a(u))}}}},BE=function(t){return function(r){return IP(t)(r)(tt(rt))}};var RP=function(t){return t};var WE=function(t){return t};var P_=function(t){return t.toDuration};var qE={fromDuration:Om()()(RP)(function(t){return t*1e3}),toDuration:Om()()(WE)(function(t){return t/1e3})};var HE=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var LP=function(t){return t};var Gc={map:wE},Ai={map:OE};var BP=function(){var t=function(n){if(n instanceof Xt)return n.value0;if(n instanceof jt)return Qa("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof jt)return n.value0;if(n instanceof Xt)return Qa("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof jt)return!0;if(n instanceof Xt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:jt.create,right:Xt.create}}(),UP=function(t){return RE(BP,t)},ko=function(t){return function(){var e=UP(t)();return e.run(),e}},zo=function(){var t=Ar(F);return function(r){return t(ko(r))}}();var ki={apply:PE,Functor0:function(){return Gc}};var QD={Applicative0:function(){return ya},Bind1:function(){return Le}},Le={bind:$E,Apply0:function(){return KD(0)}},ya={pure:FE,Apply0:function(){return KD(0)}},KD=HE("applyAff","Effect.Aff",function(){return{apply:ju(QD),Functor0:function(){return Ai}}}),zE=KD(71);var Ne={liftEffect:ME,Monad0:function(){return QD}},GE=function(){var t=be(Ne);return function(r){return LP(x(t(r)))}}(),VE=function(t){return Hc(function(r){return _(F)(GE)(t.join(r))})};var JE=function(t){return function(r){return H(Le)(be(Ne)(r.isSuspended))(function(e){return e?be(Ne)(Ar(F)(r.kill(t,x(S(f)(void 0))))):Hc(function(n){return _(F)(GE)(r.kill(t,n))})})}};var Pn={parallel:lt,sequential:NE,Monad0:function(){return QD},Applicative1:function(){return WP(0)}},WP=HE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=wn(Pn),r=S(ya);return function(e){return t(r(e))}}(),Apply0:function(){return ki}}});var qP={append:function(t){return function(r){return function(e){return BE(Pn)(Gt)([t(e),r(e)])}}}};var HP=x(S(ya)(void 0)),jE={mempty:HP,Semigroup0:function(){return qP}};var XE={alt:IE,Functor0:function(){return Gc}};var QE=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),I_=function(){function t(){}return t.value=new t,t}(),qf=function(){function t(){}return t.value=new t,t}(),R_=function(){function t(){}return t.value=new t,t}(),Hf=function(){function t(){}return t.value=new t,t}(),N_=function(){function t(){}return t.value=new t,t}(),L_=function(){function t(){}return t.value=new t,t}(),KE=function(){function t(){}return t.value=new t,t}(),Rs=function(){function t(){}return t.value=new t,t}(),Ns=function(){function t(){}return t.value=new t,t}(),B_=function(){function t(){}return t.value=new t,t}(),U_=function(){function t(){}return t.value=new t,t}(),YE=function(){function t(){}return t.value=new t,t}(),Vc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),YD=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var zP="numeric",GP="sudden",VP="unit",JP="cancel",jP="step",XP="linear",QP="exponential",KP="envelope",ZE=function(t,r,e,n){if(e.type===GP)t.value=e.value.n;else if(e.type===VP)r.id&&ZP(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===zP)t[e.value.t.type===jP?"setValueAtTime":e.value.t.type===XP?"linearRampToValueAtTime":e.value.t.type===QP?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===JP)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===KP){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},YP=function(t,r,e,n,a){return n[e]||(n[e]={}),ZE(r.parameters.get(e),n[e],a,t)},qu=function(t,r,e,n,a){return n[e]||(n[e]={}),ZE(r[e],n[e],a,t)},Te=function(t,r,e){let n=r.value0?r.value0:"@fan@";e.scopes[n]||(e.scopes[n]=[]),e.scopes[n].push(t),e.units[t].scope=n},Se=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},xe=function(t,r,e,n){t()(a=>t0(r,a,n))(e)},t0=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function ZD(t){return function(r){return function(){delete r.units[t.id]}}}function td(t){return function(r){return function(){t0(t.from,t.to,r)}}}var ZP=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function rd(t){return function(r){return function(){var e=t.from,n=t.to;if(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@")return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var ed=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},nd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),i=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:i,main:e.context.createGain(),se:u},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},ad=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},od=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new ConstantSourceNode(i,o)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},id=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},fd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},cd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},ld=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},_d=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},pd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},sd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},md=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new AudioBufferSourceNode(i,o)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},vd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Dd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},dd=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var i=e.context.createMediaElementSource(a);return i};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},bd=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},yd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Ad=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},kd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var m={frequency:o.frequency,periodicWave:o.spec.type==="wave"?o.spec.value:ub(e.context)(o.spec.value.real)(o.spec.value.img)()},s=new OscillatorNode(i,m);return s},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},gd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var m={loop:o.loop,buffer:o.buffer,playbackRate:o.playbackRate};return new AudioBufferSourceNode(i,m)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(i=>i)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Cd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),i=new MediaRecorder(u.stream);a(i)(),i.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:i,main:e.context.createGain(),se:u},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Ed=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},hd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Td=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},Sd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},xd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Fd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)},Od=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},Te(n,r.scope,e),Se(n,e),xe(t,n,r.parent,e)};function $d(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function Md(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function wd(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function Pd(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;YP(r,r.units[e].main,n,r.units[e].controllers,a)}}}var Hu=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function Id(t){return function(r){return function(){var e=t.id,n=t.gain;qu(r,r.units[e].main,"gain",r.units[e].controllers,n),Hu(n,r.units[e],"gain")}}}function Rd(t){return function(r){return function(){var e=t.id,n=t.q;qu(r,r.units[e].main,"Q",r.units[e].controllers,n),Hu(n,r.units[e],"Q")}}}function Nd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function Ld(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function Bd(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function Ud(t){return function(r){return function(){var e=t.id,n=t.pan;qu(r,r.units[e].main,"pan",r.units[e].controllers,n),Hu(n,r.units[e],"pan")}}}function Wd(t){return function(r){return function(){var e=t.id,n=t.threshold;qu(r,r.units[e].main,"threshold",r.units[e].controllers,n),Hu(n,r.units[e],"threshold")}}}function qd(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function Hd(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function zd(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function Gd(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function Vd(t){return function(r){return function(){var e=t.id,n=t.release;qu(r,r.units[e].main,"release",r.units[e].controllers,n),Hu(n,r.units[e],"release")}}}function Jd(t){return function(r){return function(){var e=t.id,n=t.offset;qu(r,r.units[e].main,"offset",r.units[e].controllers,n),Hu(n,r.units[e],"offset")}}}function jd(t){return function(r){return function(){var e=t.id,n=t.ratio;qu(r,r.units[e].main,"ratio",r.units[e].controllers,n),Hu(n,r.units[e],"ratio")}}}function Xd(t){return function(r){return function(){var e=t.id,n=t.attack;qu(r,r.units[e].main,"attack",r.units[e].controllers,n),Hu(n,r.units[e],"attack")}}}function Qd(t){return function(r){return function(){var e=t.id,n=t.knee;qu(r,r.units[e].main,"knee",r.units[e].controllers,n),Hu(n,r.units[e],"knee")}}}function Kd(t){return function(r){return function(){var e=t.id,n=t.delayTime;qu(r,r.units[e].main,"delayTime",r.units[e].controllers,n),Hu(n,r.units[e],"delayTime")}}}function Yd(t){return function(r){return function(){var e=t.id,n=t.playbackRate;qu(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),Hu(n,r.units[e],"playbackRate")}}}function Zd(t){return function(r){return function(){var e=t.id,n=t.frequency;qu(r,r.units[e].main,"frequency",r.units[e].controllers,n),Hu(n,r.units[e],"frequency")}}}function tb(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?tI(e)(n)(r)():n.x.type==="off"&&rI(e)(n)(r)()}}}var tI=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},rI=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function rb(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function Ls(t){return function(){t.stop()}}function eb(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function nb(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function W_(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function ab(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var ub=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function nf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function ob(t){return function(){t.close()}}function ib(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function fb(t){return function(r){return function(){return t.decodeAudioData(r)}}}function cb(){return new(window.AudioContext||window.webkitAudioContext)}function lb(t){return function(){return t.state}}function q_(t){return function(){return t.currentTime}}function r0(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var aI=function(t){return function(r){return Hc(function(e){return Z_(F)(wt(jE))(r0(r)(function(n){return e(jt.create(t(n)))()})(function(n){return e(Xt.create(n))()}))})}};var uI=function(t){return Ra(function(r){return Lo("Promise failed, couldn't extract JS Error or String")})(tt(rt))(pD(N(_D(Jm)(Qu))(yD(Qu)("Error")(t))(_(b_(So))(Lo)(AD(Qu)(t)))))},e0=aI(uI),Bs=function(t){return H(Le)(be(Ne)(t))(e0)};function _b(t){return function(){return URL.createObjectURL(t)}}var n0=function(t){return function(r){return function(e){return xt(eb(t))(e)(function(){var n=Qn(Tn)(r);return function(a){return n(_b(a))}}())}}};var zf={ids:_(F)(zt(rp))(Uu),deleteFromCache:ZD,disconnectXFromY:rd,connectXToY:td,makeAllpass:ed(Yt),makeAnalyser:nd(Yt),makeAudioWorkletNode:ad(Yt),makeBandpass:ud(Yt),makeConstant:od(Yt),makeConvolver:id(Yt),makeDelay:fd(Yt),makeDynamicsCompressor:cd(Yt),makeGain:ld(Yt),makeHighpass:_d(Yt),makeHighshelf:pd(Yt),makeIIRFilter:sd(Yt),makeLoopBuf:md(Yt),makeLowpass:vd(Yt),makeLowshelf:Dd(Yt),makeMediaElement:dd(Yt),makeMicrophone:bd(Yt),makeNotch:yd(Yt),makePeaking:Ad(Yt),makePeriodicOsc:kd(Yt),makePlayBuf:gd(Yt),makeRecorder:Cd(Yt),makeSawtoothOsc:Ed(Yt),makeSinOsc:hd(Yt),makeSpeaker:Td,makeSquareOsc:xd(Yt),makeStereoPanner:Sd(Yt),makeTriangleOsc:Fd(Yt),makeWaveShaper:Od(Yt),setAnalyserNodeCb:$d,setMediaRecorderCb:Md,setWaveShaperCurve:wd,setAudioWorkletParameter:Pd,setBuffer:Nd,setConvolverBuffer:Ld,setDuration:Gd(Yt),setPeriodicOsc:Bd,setOnOff:tb,setBufferOffset:zd,setLoopStart:qd,setLoopEnd:Hd,setRatio:jd,setOffset:Jd,setAttack:Xd,setGain:Id,setQ:Rd,setPan:Ud,setThreshold:Wd,setRelease:Vd,setKnee:Qd,setDelay:Kd,setPlaybackRate:Yd,setFrequency:Zd},Et=function(t){return function(r){return H(Le)(Bs(ib(r)))(function(){var e=fb(t);return function(n){return Bs(e(n))}}())}},H_=function(t){var r=be(t);return function(e){return r(lb(e))}};var ua=function(t){return be(t)(cb)},zu=function(t){var r=be(t);return function(e){return r(ab(e))}},Cn=function(t){return function(r){return be(t)(function(){var n=H_(fe)(r)();return Rn(f)(n!=="closed")(ob(r))()})}},lI=lt,_I=lt,Us=function(t){return function(r){return _(Ai)(function(e){return{microphone:function(){return t?S(To)(lI(e)):V.value}(),camera:function(){return r?S(To)(_I(e)):V.value}()}})(Bs(nb(t)(r)))}};var Go=function(){function t(){}return t.value=new t,t}(),Vo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Su=function(){function t(){}return t.value=new t,t}(),ln=XD,gi=function(t){return Ao(Pn)(N(XE)(wn(Pn)(H(Le)(VE(t))(be(Ne))))(wn(Pn)(JE(Lo("We navigated away from the page"))(t))))},Jc=function(t){return function(r){return function(e){return function(n){return N(t)(Y(r)(Su.value))(n)}}}},$a=function(t){return function(r){return function(e){return function(n){return N(t)(Y(r)(Z(de)(me.value)(Yr(x(n)))))(_(t.Functor0())(function(a){return Z(de)(me.value)(Yr(x(X(nt)(a)(n))))})(_(t.Functor0())(function(a){return a.value0})(e)))}}}},Ws=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return _(t)(function(s){return Z(de)(me.value)(Yr(x(function(){if(s.value0 instanceof Go)return S(f)(void 0);if(s.value0 instanceof Vo)return X(nt)(X(nt)(s.value0.value0)(n(S(f)(void 0))))(a(Su.value));if(s.value0 instanceof Su)return function(){s.value1(),a(Go.value)();var v=ko(H(Le)(ua(Ne))(function(D){return H(Le)(zu(Ne)(D))(function(c){return H(Le)(u(D))(function(C){return be(Ne)(function(){var mt=i(D)(C)(),vr=X(nt)(X(nt)(mt)(c))(Cn(fe)(D));return a(new Vo(vr))(),vr})})})}))();return ae(r)(Tn)(n(function(){return a(Su.value)(),zo(gi(v))()}))(function(){return S(f)(void 0)})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[s.value0.constructor.name])}())))})(On(e)(N(e.Plus0().Alt0())(Y(e)(S(f)(void 0)))(_(t)(function(s){return s.value0})(o)))(_(t)(at.create)(m)))}}}}}}}}},Ma=function(t){return function(r){return function(e){return function(){return t(e)(),r(new QE(e))()}}}},qs=function(t){return function(r){return function(e){return function(n){return function(a){return We(k)(function(u){return function(i){var o=Jc(I(f))(T(k))(r)(i);return bc(p)(N(I(f))(Y(T(k))(Z(Hp)(Vt.value)("cursor: pointer;")))(Ws(g)(jr)(T(k))(e)(u)(n)(a)(r)(o)))([un(ur)(_(g)(function(m){if(m instanceof Su)return t;if(m instanceof Go)return"\u23F3";if(m instanceof Vo)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[m.constructor.name])})(o))])}})}}}}},Tt=function(t){return function(r){return function(e){return function(n){return We(k)(function(a){return function(u){var i=Jc(I(f))(T(k))(t)(u);return gn(p)(Ws(g)(jr)(T(k))(r)(a)(e)(n)(t)(i))([un(ur)(_(g)(function(o){if(o instanceof Su)return"Turn on";if(o instanceof Go)return"Loading...";if(o instanceof Vo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[o.constructor.name])})(i))])}})}}}};var jc=function(t){return function(r){return function(){var n=nf(t)(),a=Ot(Uf([new hf(_(g)(function(u){return Ef.create(YA(u))})(r))])(zf))(function(u){return u(n)})();return a}}};var Dt=function(t){return function(r){return function(){var n=nf(t)(),a=Ot(Uf(r)(zf))(function(u){return u(n)})();return a}}},Hs=function(t){return function(){var e=ua(fe)();return _(F)(function(n){return X(nt)(n)(Cn(fe)(e))})(Dt(e)(t))()}};var pI=function(){return d.value}(),a0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(p))(d.value)(pI)({allpass:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(lr(Cr)(a)(_t()))(function(u){return function(i){return Pt(ft)(.2)([u,$_(zD)(700)([$_(xs(Ct(gt()(J(J(kt)($D)()()()({reflectSymbol:function(){return"q"}}))(Ts)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:990,q:20})([u]),$_(zD)(1110)([u,$_(xs(Ct(gt()(J(J(kt)($D)()()()({reflectSymbol:function(){return"q"}}))(Ts)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2010,q:30})([u])])])])}})])}})))})}}};function Ci(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,i=t.BYTES_PER_ELEMENT,o=Math.min(u,n>>>0);if(a===null)return new t(e,o);var m=Math.min((u-o)/i,a);return new t(e,o,m)}}var mI=Ci(Uint8ClampedArray),vI=Ci(Uint32Array),DI=Ci(Uint16Array),u0=Ci(Uint8Array),dI=Ci(Int32Array),bI=Ci(Int16Array),yI=Ci(Int8Array),AI=Ci(Float32Array),kI=Ci(Float64Array);function o0(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var zs={create:u0,BinaryValue0:function(){}};var Gs=function(t){return function(r){return function(){return o0(r)}}};var Xc=ku,Qc=ku,Kc=ku,eu=ku,nu=ku,au=ku,uu=ku,ou=ku;function Vs(t){return t|0}var Ei=function(){return window};function l0(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var i=Object.getPrototypeOf(u),o=i.constructor.name;if(o===e)return r(n);if(o==="Object")return t;u=i}return t}var It=function(t){return function(r){return l0(V.value,U.create,t,r)}};var pb=It("HTMLCanvasElement");function _0(t){return function(){return t.body}}var p0=function(){var t=_(F)(Ze);return function(r){return t(_0(r))}}();var s0=lt;function Gf(t){return function(){return t.valueAsNumber}}var Yc=It("HTMLInputElement");function mb(t){return function(){return t.document}}function Js(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var vb=lt;var fR=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},af=Bt(function(t){return function(){var e=Ei(),n=se(!0)(),a=fR("fx","FRP.Event.Animate",function(){return Ar(F)(xt(Js)(e)(function(){var o=Re(n)();return Rn(f)(o)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),Sn(!1)(n)}});var cR="background-color: rgb(150,30,10);",lR="background-color: rgb(130,60,10);",_R="background-color: rgb(80,90,10);",pR="background-color: rgb(10,130,10);",sR="background-color: rgb(10,100,0);",mR=ps(Za)(function(t){return wr(Mr(ls)()(Ha)()(D_))(cR)(wr(Mr(aa)()(vn)()(Ha))(lR)(wr(Mr(gu)()(Dn)()(vn))(_R)(wr(Mr(Cu)()(dn)()(Dn))(pR)(wr(Mr(Eu)()(hu)()(dn))(sR)(Wu)))))}),vR=function(t){return function(r){return function(e){return function(n){return ws(Ms(Ct(gt()(J(J(kt)($s)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(At()())))({cb:n,fftSize:As.value})([lr(r)(e)(_t())])}}}},DR=function(){return d.value}(),Rr="background-color: rgb(255,255,255,0.0);",Lr=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return function(l){return _(t)(function(v){var D=d_(r)()(d_(n)()(v)(m))(s);return D?Z(u)(Vt.value)(d_(r)()(d_(n)()(mR)(m))(s)):Z(u)(Vt.value)(Rr)})(l)}}}}}}}}}}},dR=function(){return 15/40}(),bR=function(){return 10/40}(),yR=function(){return 7/40}(),AR=function(){return 3/40}(),kR=function(){return 1/40}(),v0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(p))(p)(DR)({analyser:L(et(We(k)(function(n){return function(a){var u=Ll(bu(f))(tt(rt))(a),i=Jc(I(f))(T(k))(e)(function(m){return m.right}(u)),o=function(m){return m.left}(u);return qr(p)([gn(p)(N(I(f))(Y(T(k))(Z(Dc)(Vt.value)("cursor: pointer;")))(Ws(g)(jr)(T(k))(t)(function(m){return n(Xt.create(m))})(function(m){return Et(m)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(m){return function(s){return function(){var v=se(V.value)(),D=nf(m)(),c=Uf([vR(Os)(Cr)(s)(function(ut){return function(){return Sn(new U(ut))(v)(),Sn(V.value)(v)}})])(zf),C=Ot(N(I(f))(_(g)(Xt.create)(c))(_(g)(jt.create)(af)))(function(ut){if(ut instanceof Xt)return ut.value0(D);if(ut instanceof jt)return function(){var vr=Re(v)();return _a(f)(Kr)(vr)(function(nr){return function(){var le=W_(nr)(),pt=Gs(zs)(le)(),Dr=se(0)(),Tr=se(0)(),ar=se(0)(),ze=se(0)(),ho=se(0)(),Gu=se(0)(),$i=se(0)(),cu=se(0)(),_n=se(0)(),Xo=se(0)(),Jf=function(Me){if(Me<32)return Dr;if(Me<64)return Tr;if(Me<96)return ar;if(Me<128)return ze;if(Me<168)return ho;if(Me<160)return Gu;if(Me<224)return $i;if(ne)return cu;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[Me.constructor.name])};Al(pt)(function(Me){var Qo=Vs(Me);return function(){var Ko=Re(Xo)();return Af(Br(Fu)(Qo))(_n)(),Af(Br(Fu)(Qo))(Jf(Ko))(),Af(Br(Fu)(1))(Xo)()}})();var jf=Bn(Mg)(f)(function(Me){return function(){var Xf=_(F)(Gr)(Re(Me))(),Ko=_(F)(Ku(yl)(Xf))(_(F)(Gr)(Re(_n)))();return wr(Mr(ls)()(Ha)()(D_))(Ko>dR)(wr(Mr(aa)()(vn)()(Ha))(Ko>bR)(wr(Mr(gu)()(Dn)()(vn))(Ko>yR)(wr(Mr(Cu)()(dn)()(Dn))(Ko>AR)(wr(Mr(Eu)()(hu)()(dn))(Ko>kR)(Wu)))))}})(wr(Mr(xg)()(uD)()($g))(Dr)(wr(Mr(Fg)()(oD)()(uD))(Tr)(wr(Mr(Og)()(D_)()(oD))(ar)(wr(Mr(ls)()(Ha)()(D_))(ze)(wr(Mr(aa)()(vn)()(Ha))(ho)(wr(Mr(gu)()(Dn)()(vn))(Gu)(wr(Mr(Cu)()(dn)()(Dn))($i)(wr(Mr(Eu)()(hu)()(dn))(cu)(Wu)))))))))();return n(new jt(jf))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[ut.constructor.name])})();return function(){return C(),function(){var vr=H_(fe)(m)();return Rn(f)(vr!=="closed")(Cn(fe)(m))()}(),n(new jt(ps(Za)(x(ps(xa)(x(!1))))))()}}}})(e)(i)))([un(ur)(_(g)(function(m){if(m instanceof Su)return"Turn on";if(m instanceof Go)return"Loading...";if(m instanceof Vo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[m.constructor.name])})(i))]),Sr(p)(Y(T(k))(Z(st)(Vt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)($e)(bo)(st)(ba)(bo)(ou)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(Gn)(Do)(st)(ba)(Do)(uu)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(zn)(vo)(st)(ba)(vo)(au)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(Hn)(mo)(st)(ba)(mo)(nu)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(qn)(so)(st)(ba)(so)(eu)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(xa)(po)(st)(ba)(po)(Kc)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(Uo)(_o)(st)(ba)(_o)(Qc)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)($e)(ba)(Bo)(lo)(st)(ba)(lo)(Xc)(ou)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)($e)(bo)(st)(da)(bo)(ou)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(Gn)(Do)(st)(da)(Do)(uu)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(zn)(vo)(st)(da)(vo)(au)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(Hn)(mo)(st)(da)(mo)(nu)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(qn)(so)(st)(da)(so)(eu)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(xa)(po)(st)(da)(po)(Kc)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(Uo)(_o)(st)(da)(_o)(Qc)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Gn)(da)(Bo)(lo)(st)(da)(lo)(Xc)(uu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)($e)(bo)(st)(Da)(bo)(ou)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(Gn)(Do)(st)(Da)(Do)(uu)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(zn)(vo)(st)(Da)(vo)(au)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(Hn)(mo)(st)(Da)(mo)(nu)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(qn)(so)(st)(Da)(so)(eu)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(xa)(po)(st)(Da)(po)(Kc)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(Uo)(_o)(st)(Da)(_o)(Qc)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(zn)(Da)(Bo)(lo)(st)(Da)(lo)(Xc)(au)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)($e)(bo)(st)(va)(bo)(ou)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(Gn)(Do)(st)(va)(Do)(uu)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(zn)(vo)(st)(va)(vo)(au)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(Hn)(mo)(st)(va)(mo)(nu)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(qn)(so)(st)(va)(so)(eu)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(xa)(po)(st)(va)(po)(Kc)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(Uo)(_o)(st)(va)(_o)(Qc)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(Hn)(va)(Bo)(lo)(st)(va)(lo)(Xc)(nu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)($e)(bo)(st)(ma)(bo)(ou)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(Gn)(Do)(st)(ma)(Do)(uu)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(zn)(vo)(st)(ma)(vo)(au)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(Hn)(mo)(st)(ma)(mo)(nu)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(qn)(so)(st)(ma)(so)(eu)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(xa)(po)(st)(ma)(po)(Kc)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(Uo)(_o)(st)(ma)(_o)(Qc)(eu)(o)))([]),Sr(p)(N(I(f))(Y(T(k))(Z(st)(Vt.value)(Rr)))(Lr(g)(qn)(ma)(Bo)(lo)(st)(ma)(lo)(Xc)(eu)(o)))([])])])}})))})}}};var CR=function(){return d.value}(),D0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(p))(d.value)(CR)({bandpass:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(lr(Cr)(a)(_t()))(function(u){return function(i){return Pt(ft)(.8)([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var hR=function(){return d.value}(),d0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(p))(p)(hR)({compression:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([CE(lE(Ct(gt()(kt))(At()())))({})([lr(Cr)(a)(_t())])])}})))})}}};var oa=function(){return function(t){var r=nn(),e=ie()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=Nf(t);return function(a){return r(e(n(a)))}}},Vf=function(){return function(t){var r=nn(),e=ie()({reflectSymbol:function(){return"onOff"}})(d.value),n=yC(t);return function(a){return r(e(n(a)))}}},b0=function(){return function(t){var r=nn(),e=ie()({reflectSymbol:function(){return"offset"}})(d.value),n=Nf(t);return function(a){return r(e(n(a)))}}},y0=function(){var t=nn(),r=ie()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(e){return t(r(e))}},A0=function(){var t=nn(),r=ie()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(e){return t(r(e))}},En=function(){return function(t){var r=nn(),e=ie()({reflectSymbol:function(){return"gain"}})(d.value),n=Nf(t);return function(a){return r(e(n(a)))}}},go=function(){return function(t){var r=nn(),e=ie()({reflectSymbol:function(){return"frequency"}})(d.value),n=Nf(t);return function(a){return r(e(n(a)))}}};var Zc=function(){return function(t){var r=nn(),e=ie()({reflectSymbol:function(){return"delayTime"}})(d.value),n=Nf(t);return function(a){return r(e(n(a)))}}};var SR=function(){return d.value}(),k0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(B()(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}})(p))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(p))(p)(SR)({tf:L(oe(ur)("<|>")),txt:L(oe(ur)(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( bang $ offset $ AudioEnvelope
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
  ]`)),constant:L(et(Tt(e)(t)(function(n){return S(ya)(void 0)})(function(n){return function(a){return Dt(n)([Pt(ft)(.5)([Is(hs)(0)(N(I(f))(_t())(Y(T(k))(b0()(Mn)({d:5,o:.1,p:ro(Wi)(function(u){return x(function(){var i=su(xo)(u)(3)===0;return i?1:0}())})(pn(0)(1920))}))))])])}})))})}}};var FR=function(){return d.value}(),g0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(p))(p)(FR)({convolution:L(et(Tt(e)(t)(function(n){return qt(zE)(_(Ai)(function(a){return function(u){return{loop:a,verb:u}}})(Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Et(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return Dt(n)([dE(wC)(a.verb)([lr(Cr)(a.loop)(_t())])])}})))})}}};var $R=function(){return d.value}(),C0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(p))(d.value)($R)({delay:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return Dt(n)([Fa(jn(za)(a)(_t()))(function(u){return function(i){return Pt(ft)(.2)([yo(tn)(.03)([u]),yo(tn)(.1)([u]),yo(tn)(.3)([u]),yo(tn)(.7)([u])])}})])}})))})}}};var wR=function(){return d.value}(),E0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(p))(p)(wR)({gain:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return Dt(n)([Pt(ft)(.1)([lr(Cr)(a)(_t())])])}})))})}}};var IR=function(){return d.value}(),h0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(p))(p)(IR)({highpass:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Bc(tu)(2e3)([lr(Cr)(a)(_t())])])}})))})}}};var NR=function(){return d.value}(),T0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(p))(p)(NR)({highshelf:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([EE(cE(Ct(gt()(J(J(kt)(UC)()()()({reflectSymbol:function(){return"gain"}}))(WC)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,gain:.4})([lr(Cr)(a)(_t())])])}})))})}}};var BR=function(){return d.value}(),S0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})(p))(p)(BR)({iirFilterEx:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([bE()()(MC(mi)(mi))(new at(Sf()()(20298e-8)(Sf()()(.0004059599)(Sf()()(20298e-8)(Dv))),Sf()()(1.0126964558)(Sf()()(-1.9991880801)(Sf()()(.9873035442)(Dv)))))([lr(Cr)(a)(_t())])])}})))})}}};var WR=function(){return d.value}(),x0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(p))(d.value)(WR)({loopBuf:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return Dt(n)([lr(Bf(Ct(gt()(J(J(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(T_)()()()({reflectSymbol:function(){return"loopStart"}}))(h_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(_t()),lr(Bf(Ct(gt()(J(J(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(T_)()()()({reflectSymbol:function(){return"loopStart"}}))(h_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(_t()),lr(Bf(Ct(gt()(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,playbackRate:1.7})(_t())])}})))})}}};var HR=function(){return d.value}(),F0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(p))(p)(HR)({lowpass:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Uc(Ss)(215)([lr(Cr)(a)(_t())])])}})))})}}};var GR=function(){return d.value}(),O0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(p))(p)(GR)({lowshelf:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([TE(fE(Ct(gt()(J(J(kt)(NC)()()()({reflectSymbol:function(){return"gain"}}))(LC)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:91,gain:.4})([lr(Cr)(a)(_t())])])}})))})}}};var JR=function(){return d.value}(),$0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(p))(d.value)(JR)({microphone:L(et(Tt(e)(t)(function(n){return Us(!0)(!1)})(function(n){return function(a){return Dt(n)([function(){if(a.microphone instanceof U)return ru(function(u){return Pt(ft)(1)([F_(C_)(a.microphone.value0),yo(tn)(.1)([Pt(ft)(.2)([u])])])});if(a.microphone instanceof V)return Pt(ft)(.02)([kE(ef)(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[a.microphone.constructor.name])}()])}})))})}}};var XR=function(){return d.value}(),M0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(p))(p)(XR)({notch:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Wc(Lc(Ct(gt()(J(J(kt)(Pc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1})(S(pe)(Wc(Lc(Ct(gt()(J(J(kt)(Pc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5})(S(pe)(Wc(Lc(Ct(gt()(J(J(kt)(Pc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10})(S(pe)(Wc(Lc(Ct(gt()(J(J(kt)(Pc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})(S(pe)(Wc(Lc(Ct(gt()(J(J(kt)(Pc)()()()({reflectSymbol:function(){return"q"}}))(Ic)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30})(S(pe)(lr(Cr)(a)(_t())))))))))))])}})))})}}};var KR=function(){return d.value}(),w0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(p))(p)(KR)({peaking:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([qc(Nc(Ct(gt()(J(J(J(kt)($c)()()()({reflectSymbol:function(){return"q"}}))(Mc)()()()({reflectSymbol:function(){return"gain"}}))(wc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1,gain:-20})(S(pe)(qc(Nc(Ct(gt()(J(J(J(kt)($c)()()()({reflectSymbol:function(){return"q"}}))(Mc)()()()({reflectSymbol:function(){return"gain"}}))(wc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5,gain:20})(S(pe)(qc(Nc(Ct(gt()(J(J(J(kt)($c)()()()({reflectSymbol:function(){return"q"}}))(Mc)()()()({reflectSymbol:function(){return"gain"}}))(wc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10,gain:-20})(S(pe)(qc(Nc(Ct(gt()(J(J(J(kt)($c)()()()({reflectSymbol:function(){return"q"}}))(Mc)()()()({reflectSymbol:function(){return"gain"}}))(wc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20,gain:20})(S(pe)(qc(Nc(Ct(gt()(J(J(J(kt)($c)()()()({reflectSymbol:function(){return"q"}}))(Mc)()()()({reflectSymbol:function(){return"gain"}}))(wc)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30,gain:-20})(S(pe)(lr(Cr)(a)(_t())))))))))))])}})))})}}};var ZR=function(){return d.value}(),P0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(ZR)({periodic:L(et(Tt(e)(t)(function(n){return S(ya)(void 0)})(function(n){return function(a){return Dt(n)([Pt(ft)(.2)([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:140,spec:new at(wr(Mr(aa)()(vn)()(Ha))(.1)(wr(Mr(gu)()(Dn)()(vn))(.2)(wr(Mr(Cu)()(dn)()(Dn))(.3)(wr(Mr(Eu)()(hu)()(dn))(.4)(Wu)))),wr(Mr(aa)()(vn)()(Ha))(.4)(wr(Mr(gu)()(Dn)()(vn))(.3)(wr(Mr(Cu)()(dn)()(Dn))(.2)(wr(Mr(Eu)()(hu)()(dn))(.1)(Wu)))))})(_t())])])}})))})}}};var rN=function(){return d.value}(),I0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(K(f))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(p))(p)(rN)({playBuf:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return Dt(n)([jn(S_(Ct(gt()(J(J(J(kt)(IC)()()()({reflectSymbol:function(){return"duration"}}))(PC)()()()({reflectSymbol:function(){return"bufferOffset"}}))(E_)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:a,duration:3,bufferOffset:4.2})(_t())])}})))})}}};var Db=function(){function t(){}return t.value=new t,t}();var R0={attr:function(t){return function(r){return b({key:"controls",value:q(r)})}}};var db=function(){function t(){}return t.value=new t,t}();var N0={attr:function(t){return function(r){return b({key:"src",value:q(r)})}}};var bb=function(t){return function(r){return function(e){return new P(j(t)("audio")(r)(W(e)))}}};var oN=function(t){return function(r){return function(e){return function(n){return VD(t)(n)(F_(r)(e))}}}},iN=function(){return d.value}(),L0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(p))(p)(iN)({recorder:L(et(We(k)(function(n){return function(a){var u=Ll(bu(f))(tt(rt))(a),i=Ll(bu(f))(tt(rt))(function(l){return l.left}(u)),o=function(l){return l.right}(i),m=Jc(I(f))(T(k))(e)(function(l){return l.right}(u)),s=function(l){return l.left}(i);return qr(p)([gn(p)(N(I(f))(Y(T(k))(Z(Dc)(Vt.value)("cursor: pointer;")))(_(g)(function(l){return Z(de)(me.value)(Yr(x(function(){if(l.e instanceof Go)return S(f)(void 0);if(l.e instanceof Vo)return X(nt)(X(nt)(X(nt)(l.e.value0)(t(S(f)(void 0))))(_a(f)(Kr)(l.rec)(function(){var v=i_(Vv);return function(D){return v(Ls(D))}}())))(n(Xt.create(Su.value)));if(l.e instanceof Su)return function(){l.cncl();var D=x_();n(new Xt(Go.value))();var c=ko(H(Le)(_(Ai)(function(C){return C.microphone})(Us(!0)(!1)))(function(C){return be(Ne)(function(){var mt=Yt(S(f)(S(f)(void 0)))(function(vr){return function(){var Zr=ua(fe)(),le=nf(Zr)(),pt=Uf([oN(SD)(C_)(vr)(function(Tr){return function(){return n(new jt(new Xt(Tr)))(),Ar(F)(Fs(Tr)(D))(),n0("audio/ogg; codecs=opus")(function(ze){return n(jt.create(jt.create(ze)))})(Tr)()}})])(zf),Dr=Ot(pt)(function(Tr){return Tr(le)})();return function(){Dr(),H(Tn)(vE(D))(ve(f)(Kr)(function(){var ze=i_(Vv);return function(ho){return ze(Ls(ho))}}()))();var ar=H_(fe)(Zr)();return Rn(f)(ar!=="closed")(Cn(fe)(Zr))()}}})(C)();return n(new Xt(new Vo(mt)))(),mt})}))();return t(function(){return n(Xt.create(Su.value))(),zo(gi(c))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[l.e.constructor.name])}())))})(On(T(k))(N(I(f))(Y(T(k))(V.value))(_(g)(U.create)(o)))(_(g)(ml)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(l){return l.value0})(e)))(_(g)(function(l){return function(v){return function(D){return{e:l,cncl:v,rec:D}}}})(m)))))))([un(ur)(_(g)(function(l){if(l instanceof Su)return"Turn on";if(l instanceof Go)return"Loading...";if(l instanceof Vo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[l.constructor.name])})(m))]),qr(p)([bb(p)(N(I(f))(Y(T(k))(Z(R0)(Db.value)("true")))(N(I(f))(Y(T(k))(Z(Ov)(Vt.value)("display:none;")))(N(I(f))(_(g)(function(l){return Z(N0)(db.value)(l)})(s))(_(g)(x(Z(Ov)(Vt.value)("display:block;")))(s)))))([])])])}})))})}}};var cN=function(){return d.value}(),B0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(cN)({periodic:L(et(Tt(e)(t)(function(n){return S(ya)(void 0)})(function(n){return function(a){return Dt(n)([Pt(ft)(.2)([AE($C)(448)(_t())])])}})))})}}};var _N=function(){return d.value}(),U0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(_N)({periodic:L(et(Tt(e)(t)(function(n){return S(ya)(void 0)})(function(n){return function(a){return Dt(n)([Pt(ft)(.2)([Wf(ef)(448)(_t())])])}})))})}}};var sN=function(){return d.value}(),W0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(sN)({periodic:L(et(Tt(e)(t)(function(n){return S(ya)(void 0)})(function(n){return function(a){return Dt(n)([Pt(ft)(.2)([O_(Oc)(448)(_t())])])}})))})}}};var vN=function(){return d.value}(),q0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(p))(p)(vN)({pan:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return Dt(n)([SE(OC)(1)([lr(Cr)(a)(_t())])])}})))})}}};var dN=function(){return d.value}(),H0=Mt({reflectType:function(){return`<ul>
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
`}})()()(K(f))(p)(dN)({});var yN=function(){return d.value}(),z0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(B()(K(f))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(p))(p)(yN)({periodic:L(et(Tt(e)(t)(function(n){return S(ya)(void 0)})(function(n){return function(a){return Dt(n)([Pt(ft)(.2)([Ps(Es)(448)(_t())])])}})))})}}};var kN=function(){return d.value}(),G0=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(B()(B()(K(f))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(p))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(p))(p)(kN)({code:L(oe(ur)(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(i){var o=Zi/180;return _(Or)(function(m){var s=Gr(m)*2/Gr(44100)-1;return(3+i)*s*20*o/(Zi+i*sm(Ia)(mf)(s))})(pn(0)(44099))};return Dt(n)([xE(iE)(rb(u(400)))([lr(Cr)(a)(_t())])])}})))})}}};var CN=function(){return d.value}(),V0=function(t){return function(r){return function(e){return function(n){var a=X(nt)(r(Hf.value))(ln),u=Ma(t)(e);return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(on()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(B()(K(f))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(p))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}})(p))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}})(p))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}})(p))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}})(p))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}})(p))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(p))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(p))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}})(p))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(p))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(p))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(p))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(p))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(p))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(p))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}})(p))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(p))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(p))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(p))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}})(p))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(p))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(p))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(p))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(p))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(p))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(p))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(p))(p)(CN)({drumroll:L(et(qs("\u{1F941}")(n)(u)(function(i){return Et(i)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(i){return function(o){return Dt(i)([Pt(ft)(1)([lr(Cr)(o)(_t())])])}}))),toc:L(H0),allpass:L(a0(u)(r)(n)),analyser:L(v0(u)(r)(n)),bandpass:L(D0(u)(r)(n)),constant:L(k0(u)(r)(n)),compression:L(d0(u)(r)(n)),convolution:L(g0(u)(r)(n)),delay:L(C0(u)(r)(n)),gain:L(E0(u)(r)(n)),highpass:L(h0(u)(r)(n)),highshelf:L(T0(u)(r)(n)),iirFilter:L(S0(u)(r)(n)),loopBuf:L(x0(u)(r)(n)),lowshelf:L(O0(u)(r)(n)),lowpass:L(F0(u)(r)(n)),notch:L(M0(u)(r)(n)),playBuf:L(I0(u)(r)(n)),peaking:L(w0(u)(r)(n)),microphone:L($0(u)(r)(n)),pan:L(q0(u)(r)(n)),periodicOsc:L(P0(u)(r)(n)),recorder:L(L0(u)(r)(n)),sawtoothOsc:L(B0(u)(r)(n)),sinOsc:L(U0(u)(r)(n)),squareOsc:L(W0(u)(r)(n)),triangleOsc:L(z0(u)(r)(n)),waveShaper:L(G0(u)(r)(n)),next:$a(I(f))(T(k))(n)(a)})}}}};var yb=function(){function t(){}return t.value=new t,t}(),J0={attr:function(t){return function(r){return b({key:"checked",value:q(r)})}}};var Co=function(){function t(){}return t.value=new t,t}();var Jo={attr:function(t){return function(r){return b({key:"type",value:q(r)})}}};var Eo=function(t){return function(r){return function(e){return new P(j(t)("input")(r)(W(e)))}}};var SN=function(t){return t},Ks=function(t){return function(r){return function(e){return uo(t)(N(t.Plus0().Alt0())(Y(t)(r))(e))}}};var V_=function(t){return function(r){return t(r)}},uf=function(t){return{map:function(r){return function(e){return function(n){return e(_(t)(function(a){return function(u){return a(r(u))}})(n))}}}}},hi=function(t){return function(r){return function(e){return function(n){return V_(_(uf(t.Filterable1().Functor1()))(r)(e))(_(t.Filterable1().Functor1())(Qf)(n))}}}};var tl=function(t){return hi(t)(x)};var iu=SN;var j0=function(t){return function(r){return iu(function(e){return Oe(T(k))(N(I(f))(Y(T(k))(V_(t)(e)))(_(g)(function(n){return V_(n)(e)})(r)))})}},Ab=function(t){return{apply:function(r){return function(e){return function(n){return e(r(_(t)(Mu(Yo))(n)))}}},Functor0:function(){return uf(t)}}};var rl=function(t){return function(r){return Bt(function(e){return Ot(r)(function(n){return function(){var u=q_(t)();return e({acTime:u,value:n})()}})})}};var X0=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return function(){var v=Re(i)();return Rn(f)(v)(function(){var c=q_(t)(),C=xp(Gk($u(Ia)(u-c-.04)(.01)*1e3))(function(){var mt=Re(i)();return Rn(f)(mt)(function(){return Sn(u)(m)(),a(u)(),n(a)(u+s)(i)(o)(m)(s)()})()})();return Sn(new U(C))(o)()})()}}}}}}};return Bt(function(a){return function(){var i=se(!0)(),o=se(V.value)(),m=q_(t)(),s=se(m+r)();n(a)(r)(i)(o)(s)(r)();var l=Ot(e)(function(v){return function(){H(Tn)(Re(o))(ve(f)(Kr)(Wl))();var c=Re(s)();return n(a)(c+v)(i)(o)(s)(v)()}})();return X(nt)(X(nt)(l)(Sn(!1)(i)))(H(Tn)(Re(o))(ve(f)(Kr)(Wl)))}})}}};var wa=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var i=(n-r)/(e-t),o=r-i*t;return i*a+o}}}}};var xN=function(){return d.value}(),Q0=function(t){return function(r){return function(e){return function(n){return Mt({reflectType:function(){return`<section>
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

</section>`}})()()(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(p))(p)(xN)({txt:L(oe(ur)(`module Main where

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
import FRP.Event.Class (bang, fold, mapAccum, sampleOn)
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
        startE = bang unit <|> event.startStop.start
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
                        bang (D.Xtype := "checkbox")
                        bang (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:L(et(Ya()(k)(Nu({reflectSymbol:function(){return"cbx"}})()()()(ue({reflectSymbol:function(){return"cbx0"}})()()(ue({reflectSymbol:function(){return"cbx1"}})()()(ue({reflectSymbol:function(){return"cbx2"}})()()(ue({reflectSymbol:function(){return"cbx3"}})()()(Wn)()()()())()()()())()()()())()()()())(Nu({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(Wn)()()()())()()()())(d.value)(function(a){return function(u){var i=N(I(f))(Y(T(k))(void 0))(u.startStop.start),o=function(D){return Ks(T(k))(!1)(Iu(T(k))(x(Du(La)))(D)(!1))},m=o(u.cbx.cbx3),s=o(u.cbx.cbx2),l=o(u.cbx.cbx1),v=o(u.cbx.cbx0);return qr(p)([gn(p)(sn(Gt)(h(f))(_(g)(function(){var D=Z(de)(me.value);return function(c){return D(Yr(x(c)))}}()))([Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(D){return D.value0})(n)))(Q(g)(i)(tt(rt))))(function(D){return function(){D();var C=ua(fe)(),ut=zu(fe)(C)(),mt=function(Zr){return function(le){return function(pt){return ql(T(k))(function(Dr){return function(Tr){var ar=Tr.value1+(Dr.value1-Tr.value0)*function(){return Dr.value0?Zr:1}();return new at(new at(Dr.value1,ar),ar)}})(hi(T(k))(at.create)(le)(pt))(new at(0,0))}}},vr=jc(C)(ji(k)(_(g)(function(){var Zr=Br(ga)(.04);return function(le){return Zr(function(pt){return pt.acTime}(le))}}())(rl(C)(af)))(function(Zr){var le=function(ze){return function(ho){return uo(T(k))(Zr)(_(g)(ml)(uo(T(k))(ho)(_(g)(function(Gu){return function($i){return function(cu){return{f:Gu,a:$i,t:cu}}}})(ze))))}},pt=_(g)(function(ze){return ze?4:1})(tl(T(k))(m)(Zr)),Dr=mt(4)(s)(Zr),Tr=_(g)(function(ze){return ze?4:1})(tl(T(k))(l)(Zr)),ar=mt(8)(v)(Zr);return[He(ft)(0)(Qr(g)(le(ar)(Tr))(function(ze){return En()(Be)({n:wa(1)(.01)(4)(.15)(ze.a)*Vp(Zi*ze.f)+.15,o:ze.t,t:Wo})}))([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:325.6,spec:new at(wr(Mr(aa)()(vn)()(Ha))(.3)(wr(Mr(gu)()(Dn)()(vn))(-.1)(wr(Mr(Cu)()(dn)()(Dn))(.7)(wr(Mr(Eu)()(hu)()(dn))(-.4)(Wu)))),wr(Mr(aa)()(vn)()(Ha))(.6)(wr(Mr(gu)()(Dn)()(vn))(.3)(wr(Mr(Cu)()(dn)()(Dn))(.2)(wr(Mr(Eu)()(hu)()(dn))(0)(Wu)))))})(he(Gt)(h(f))([_t(),Qr(g)(le(Dr)(pt))(function(ze){return go()(Be)({n:325.6+wa(1)(3)(4)(15.5)(ze.a)*Vp(Zi*ze.f),o:ze.t,t:Wo})})]))])]}))(),nr=X(nt)(X(nt)(vr)(ut))(Cn(fe)(C));return t(X(nt)(nr)(a.startStop.start(void 0)))(),a.startStop.stop(nr)()}}),Qr(g)(u.startStop.stop)(function(D){return X(nt)(D)(X(nt)(t(S(f)(void 0)))(a.startStop.start(void 0)))})]))([un(ur)(he(Gt)(h(f))([Q(g)(i)("Turn on"),Q(g)(u.startStop.stop)("Turn off")]))]),Sr(p)(sn(Gt)(h(f))(_(g)(Z(st)(Vt.value)))([Q(g)(u.startStop.stop)("display:block;"),Q(g)(i)("display:none;")]))(_(Or)(function(D){return Eo(p)(he(Gt)(h(f))([Y(T(k))(Z(Jo)(Co.value)("checkbox")),Y(T(k))(Z(de)(me.value)(Yr(x(D(void 0))))),Q(g)(i)(Z(J0)(yb.value)("false"))]))([])})(mm(Or)([function(D){return D.cbx0},function(D){return D.cbx1},function(D){return D.cbx2},function(D){return D.cbx3}])(a.cbx)))])}})))})}}}};var kb={recip:function(t){return 1/t},Ring0:function(){return mf}};var gb=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function el(t){return function(){return function(r){return t(r)()}}}function nl(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function al(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function Cb(t){return t.clientX}function Eb(t){return t.clientY}function J_(t){return t.button}var j_=It("MouseEvent");var K0=function(t){return function(r){return Bt(function(e){return Ot(r)(function(n){return function(){var u=Re(t.buttons)();return e({value:n,buttons:u})()}})})}};var Y0=function(){var r=se(V.value)(),e=se(uv)(),n=_(F)(vb)(Ei)(),a=el(function(m){return ve(f)(Kr)(function(s){return Sn(new U({x:Cb(s),y:Eb(s)}))(r)})(j_(m))})(),u=el(function(m){return ve(f)(Kr)(function(s){return tc(WA(Xe)(J_(s)))(e)})(j_(m))})(),i=el(function(m){return ve(f)(Kr)(function(s){return tc(Sp(Xe)(J_(s)))(e)})(j_(m))})();nl(nn()("mousemove"))(a)(!1)(n)(),nl(nn()("mousedown"))(u)(!1)(n)(),nl(nn()("mouseup"))(i)(!1)(n)();var o=function(){return al(nn()("mousemove"))(a)(!1)(n)(),al(nn()("mousedown"))(u)(!1)(n)(),al(nn()("mouseup"))(i)(!1)(n)()};return{position:r,buttons:e,dispose:o}},Z0=Bt(function(t){return function(){var e=_(F)(vb)(Ei)(),n=el(function(a){return ve(f)(Kr)(function(u){return t(J_(u))})(j_(a))})();return nl(nn()("mousedown"))(n)(!1)(e)(),al(nn()("mousedown"))(n)(!1)(e)}});var rh=function(t){return iu(function(r){return _(g)(function(e){return e.value(e.buttons)})(K0(t)(r))})};var Sb=function(t){return t};function tm(){return Date.now()}var Th=function(t){return Bt(function(r){return Ot(t)(function(e){return function(){var a=tm();return r({time:a,value:e})()}})})};var l1=iu(function(t){return _(g)(function(r){return r.value(r.time)})(Th(t))}),Fb=_(uf(g))(function(){var t=P_(qE);return function(r){return t(Sb(r))}}())(l1);var p1=function(t){var r=function(u){return function(i){return function(o){return function(m){return function(s){return function(l){return function(v){var D=Br(i.DivisionRing1().Ring0().Semiring0())(Ca(i.DivisionRing1().Ring0().Semiring0()))(Ca(i.DivisionRing1().Ring0().Semiring0())),c=function(C){return function(ut){if(C.last instanceof V)return ut;if(C.last instanceof U)return Br(o)(ut)(m(function(mt){return Ku(i.EuclideanRing0())(In(i.DivisionRing1().Ring0().Semiring0())(mt(Br(o)(C.last.value0.value1)(C.now.value1)))(Ou(i.DivisionRing1().Ring0())(C.now.value0)(C.last.value0.value0)))(D)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[C.constructor.name,ut.constructor.name])}};return iu(function(C){var ut=V_(v)(Q(u.Filterable1().Functor1())(C)(tt(rt))),mt=Op(u)(hi(u)(at.create)(l)(ut)),vr=Iu(u)(c)(mt)(s);return uo(u)(vr)(C)})}}}}}}},e=function(u){return function(i){return r(u)(i)(i.DivisionRing1().Ring0().Semiring0())(function(o){return o(tt(rt))})}},n=function(u){return function(i){return iu(function(o){return Hl(T(k))(function(m){var s=i(Ks(T(k))(u)(m));return{input:tl(T(k))(s)(o),output:uo(T(k))(m)(o)}})})}},a=function(u){return function(i){return function(o){if(UA(u))return-8*(i-1)-o*2;if(ne)return 2*(4-i);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,i.constructor.name,o.constructor.name])}}};return n(2)(function(u){return e(T(k))(gb(yl)(kb))(2)(_(uf(g))(Ce())(Fb))(function(){var i=n(10)(function(o){return e(T(k))(gb(yl)(kb))(10)(_(uf(g))(Ce())(Fb))(qt(Ab(g))(qt(Ab(g))(_(uf(g))(a)(rh(t)))(u))(o))});return j0(i)(Q(g)(Z0)(i))}())})},s1=function(){return d.value}(),Sh=function(t){return function(r){return function(e){return function(n){return Mt({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(p))(p)(s1)({txt:L(oe(ur)(`module Main

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
import FRP.Event.Class (class IsEvent, bang, fix, fold, sampleOn, withLast)
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
        startE = bang unit <|> event.start
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
  )`)),empl:L(et(Ya()(k)(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(d.value)(function(a){return function(u){var i=N(I(f))(Y(T(k))(void 0))(u.start);return qr(p)([gn(p)(sn(Gt)(h(f))(_(g)(function(){var o=Z(de)(me.value);return function(m){return o(Yr(x(m)))}}()))([Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(o){return o.value0})(n)))(Q(g)(i)(tt(rt))))(function(o){return function(){o();var s=ua(fe)(),l=zu(fe)(s)(),v=Y0(),D=e_(0)(1e4)(),c=function(pt){return{o:pt.value0+.04,n:pt.value1,t:Wo}},C=_(co)(function(pt){return pt-.5})(m_(Ag)),ut=H(Mf)(C)(function(pt){return H(Mf)(C)(function(Dr){return H(Mf)(C)(function(Tr){return H(Mf)(C)(function(ar){return S(p_)(wr(Mr(aa)()(vn)()(Ha))(pt)(wr(Mr(gu)()(Dn)()(vn))(Dr)(wr(Mr(Cu)()(dn)()(Dn))(Tr)(wr(Mr(Eu)()(hu)()(dn))(ar)(Wu)))))})})})}),mt=qt(wf)(_(co)(at.create)(ut))(ut),vr=qt(wf)(qt(wf)(qt(wf)(_(co)(function(pt){return function(Dr){return function(Tr){return function(ar){return{s0:pt,s1:Dr,s2:Tr,s3:ar}}}}})(mt))(mt))(mt))(mt),nr=hc(vr)({newSeed:kc(D),size:5}),Zr=jc(s)(ji(k)(_(g)(function(pt){return new at(pt.acTime,pt.value)})(rl(s)(tl(T(k))(p1(v))(af))))(function(pt){return[He(ft)(0)(_(g)(function(){var Dr=En()(Be),Tr=Vn(bn)(function(ar){return $u(Ia)(-.4)(.5*(ar-1))});return function(ar){return Dr(c(Tr(ar)))}}())(pt))([Uc(UD(Ct(gt()(J(J(kt)(BC)()()()({reflectSymbol:function(){return"q"}}))(xD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4,q:20})([gE(Oc)(90.4)])]),He(ft)(0)(_(g)(function(){var Dr=En()(Be),Tr=Vn(bn)(function(ar){return $u(Ia)(-.2)(.4*(ar-3))});return function(ar){return Dr(c(Tr(ar)))}}())(pt))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*4,q:20})([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*3.02,spec:nr.s0})(N(I(f))(_t())(_(g)(function(){var Dr=go()(Be),Tr=Vn(bn)(function(ar){return 90.4*3.02+14*(ar-1)});return function(ar){return Dr(c(Tr(ar)))}}())(pt)))])]),He(ft)(0)(_(g)(function(){var Dr=En()(Be),Tr=Vn(bn)(function(ar){return $u(Ia)(-.1)(.2*(ar-6))});return function(ar){return Dr(c(Tr(ar)))}}())(pt))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*6,q:20})([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*5.07,spec:nr.s1})(N(I(f))(_t())(_(g)(function(){var Dr=go()(Be),Tr=Vn(bn)(function(ar){return 90.4*5.07+18*(ar-1)});return function(ar){return Dr(c(Tr(ar)))}}())(pt)))])]),He(ft)(0)(_(g)(function(){var Dr=En()(Be),Tr=Vn(bn)(function(ar){return $u(Ia)(0)(.2*(ar-3))});return function(ar){return Dr(c(Tr(ar)))}}())(pt))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*8,q:20})([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*7.13,spec:nr.s2})(N(I(f))(_t())(_(g)(function(){var Dr=go()(Be),Tr=Vn(bn)(function(ar){return 90.4*7.13+32*(ar-1)});return function(ar){return Dr(c(Tr(ar)))}}())(pt)))])]),He(ft)(0)(_(g)(function(){var Dr=En()(Be),Tr=Vn(bn)(function(ar){return $u(Ia)(0)(.1*(ar-7))});return function(ar){return Dr(c(Tr(ar)))}}())(pt))([yi(bi(Ct(gt()(J(J(kt)(di(vi(aa)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:90.4*9.14,spec:nr.s3})(N(I(f))(_t())(_(g)(function(){var Dr=go()(Be),Tr=Vn(bn)(function(ar){return 90.4*9.14+31*(ar-1)});return function(ar){return Dr(c(Tr(ar)))}}())(pt)))])]}))(),le=X(nt)(X(nt)(Zr)(l))(Cn(fe)(s));return t(X(nt)(le)(a.start(void 0)))(),a.stop(le)()}}),Qr(g)(u.stop)(function(o){return X(nt)(o)(X(nt)(t(S(f)(void 0)))(a.start(void 0)))})]))([un(ur)(he(Gt)(h(f))([Q(g)(i)("Turn on"),Q(g)(u.stop)("Turn off")]))])])}})))})}}}};var v1=function(){return d.value}(),xh=function(t){return function(r){return function(e){return function(n){var a=Ma(t)(e);return Mt({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(B()(B()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}})(p))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})(p))(p)(v1)({next:$a(I(f))(T(k))(n)(X(nt)(r(U_.value))(ln)),fold:L(Q0(a)(r)(e)(n)),fix:L(Sh(a)(r)(e)(n))})}}}};var d1=function(){function t(){}return t.value=new t,t}(),Fh=function(){function t(){}return t.value=new t,t}(),Ob=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),b1=`module Main where

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
import FRP.Event.Class (bang)
import Math (pow)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = bang $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = bang $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = bang $ P.gain
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
main = runInBody1 (bus \\push -> lcmap (bang Init <|> _) \\event ->
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
`;var y1=function(){return d.value}(),A1=function(t){return function(r){return function(e){return Y(t)(Vf(r)(Rf)({x:TD,o:e}))}}},k1=function(t){return function(r){return function(e){return Y(t)(Vf(r)(Rf)({x:SC,o:e}))}}},g1=Ka(Un)(Gr)(function(t){var r=function(a){return N(I(f))(A1(T(k))()(a+.27*(t*Yi(1.005)(t))))(k1(T(k))()(a+3+.3*(t*Yi(1.005)(t))))},e=function(a){return Y(T(k))(En()(Mn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*Yi(1.005)(t)),d:.8}))},n=function(a){return function(u){return He(ft)(0)(e(a))([Wf(ef)(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),Oh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(p))(d.value)(y1)({txt:L(oe(ur)(b1)),ex0:L(et(We(k)(function(n){return Ka(Un)(function(a){return N(I(f))(Y(T(k))(d1.value))(a)})(function(a){return qr(p)([gn(p)(Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(u){return u.value0})(e)))(_(g)(at.create)(a)))(function(u){return Z(de)(me.value)(Yr(x(function(){return u.value0 instanceof Ob?X(nt)(X(nt)(u.value0.value0)(n(Fh.value)))(t(S(f)(void 0))):function(){u.value1();var o=Hs([Pt(ft)(1)(Ja(Zo)(_(Or)(g1)(pn(0)(100))))])();return t(X(nt)(o)(n(Fh.value)))(),n(new Ob(o))()}}())))}))([un(ur)(Qr(g)(a)(function(u){return u instanceof Ob?"Turn off":"Turn on"}))])])})})))})}}};var Ti=function(){function t(){}return t.value=new t,t}();var ff={attr:function(t){return function(r){return b({key:"max",value:q(r)})}}};var Si=function(){function t(){}return t.value=new t,t}();var cf={attr:function(t){return function(r){return b({key:"min",value:q(r)})}}};var xi=function(){function t(){}return t.value=new t,t}();var lf={attr:function(t){return function(r){return b({key:"input",value:ct(r)})}}};var Fi=function(){function t(){}return t.value=new t,t}(),_f={attr:function(t){return function(r){return b({key:"step",value:q(r)})}}};var Oi=function(){function t(){}return t.value=new t,t}();var pf={attr:function(t){return function(r){return b({key:"value",value:q(r)})}}};var jo=function(t){return function(r){return function(e){return N(t)(r)(e(void 0))}}};var E1=tg,fu={convert:function(t){return t}},X_={convert:function(t){return u_(t)}},Mh=function(t){return t},$b=function(t){return t.convert},Va=function(t){return function(r){return function(e){return bt(E1)(u_(r))($b(t)(e(void 0)))}}};var Q_=function(t){return function(r){return function(e){return function(n){return sn(rg)(r)(e)(Mh($b(t)(n)))}}}};function Ph(t){return t.target}var ul=function(t){return Ze(Ph(t))};var S1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn)
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
        start = event.startStop.start <|> bang unit
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
                  (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap bang O.do
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
`,x1=function(){return d.value}(),F1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",Ih=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(B()(K(f))({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}})(p))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))(d.value)(x1)({wagtxt:L(oe(ur)(`run2_
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
          (add <$> (bang 0.0 <|> sl1))`)),txt:L(oe(ur)(S1)),ex1:L(et(Ya()(k)(Nu({reflectSymbol:function(){return"slider"}})()()()(ue({reflectSymbol:function(){return"s0"}})()()(ue({reflectSymbol:function(){return"s1"}})()()(ue({reflectSymbol:function(){return"s2"}})()()(Wn)()()()())()()()())()()()())(Nu({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"loading"}})()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(a.startStop.start)(Y(T(k))(void 0)),i=function(o){return lr(Bf(Ct(gt()(J(J(J(J(kt)(Rc)()()()({reflectSymbol:function(){return"playbackRate"}}))(T_)()()()({reflectSymbol:function(){return"loopStart"}}))(h_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Lf)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:o,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(jo(I(f))(_t())(function(){return jo(I(f))(_(g)(function(){var m=oa()(Cs),s=wa(0)(.2)(100)(5);return function(l){return m(s(l))}}())(a.slider.s0))(function(){return jo(I(f))(_(g)(function(){var m=y0(),s=wa(0)(0)(100)(1.2);return function(l){return m(s(l))}}())(a.slider.s1))(function(){return _(g)(function(){var m=A0(),s=wa(0)(.05)(100)(1);return function(l){return m(s(l))}}())(On(T(k))(a.slider.s2)(_(g)(Br(ga))(N(I(f))(Y(T(k))(0))(a.slider.s1))))})})}))};return qr(p)(bt(hn)(_(Or)(function(o){return qr(p)([oe(ur)(o.l),Eo(p)(Q_(fu)(h(f))(Y(T(k)))(Va(fu)(Z(Jo)(Co.value)("range"))(function(){return Va(fu)(Z(cf)(Si.value)("0"))(function(){return Va(fu)(Z(ff)(Ti.value)("100"))(function(){return Va(fu)(Z(_f)(Fi.value)("1"))(function(){return Va(X_)(Z(pf)(Oi.value)("50"))(function(){return Z(lf)(xi.value)(Yr(function(){var m=ve(f)(Kr)(Kf(Tn)(Gf)(o.f)),s=Qn(ha)(Yc);return function(l){return m(s(ul(l)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([gn(p)(Q_(fu)(h(f))(_(g)(function(){var o=Z(de)(me.value);return function(m){return o(Yr(x(m)))}}()))(Va(fu)(Q(g)(a.startStop.loading)(S(f)(void 0)))(function(){return Va(X_)(Qr(g)(a.startStop.stop)(function(o){return X(nt)(o)(X(nt)(t(S(f)(void 0)))(n.startStop.start(void 0)))}))(function(){return Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(o){return o.value0})(e)))(Q(g)(u)(tt(rt))))(function(o){return function(){o(),n.startStop.loading(void 0)();var s=ko(H(Le)(ua(Ne))(function(l){return H(Le)(zu(Ne)(l))(function(v){return H(Le)(Et(l)(F1))(function(D){return be(Ne)(function(){var C=Dt(l)([i(D)])(),ut=X(nt)(X(nt)(C)(v))(Cn(fe)(l));return n.startStop.stop(ut)(),ut})})})}))();return t(function(){return n.startStop.start(void 0)(),zo(gi(s))()})(),void 0}})})})))([un(ur)(jo(I(f))(_(g)(x("Turn off"))(a.startStop.stop))(function(){return _(g)(x("Turn on"))(u)}))])]))}})))})}}};var $1=`module Main where

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
import FRP.Event.Class (bang)
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
        start = event.startStop.start <|> bang unit

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
                ( O.oneOfMap bang O.do
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
  )`,M1=iu(function(t){return Bt(function(r){return Ot(t)(function(e){return function(){var a=Uu();return r(e(a))()}})})}),w1=function(){return d.value}(),P1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(ne)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},Rh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(p))(d.value)(w1)({txt:L(oe(ur)($1)),ex2:L(et(Ya()(k)(ue({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(Wn)()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(a.startStop.start)(Y(T(k))(void 0)),i=function(o){return ji(k)(o)(function(m){var s=_(g)(function(){var ut=Br(ga)(.01);return function(mt){return ut(en(mt))}}())(m),l=_(g)(Ua)(m),v=N(I(f))(_t())(_(g)(function(){var ut=go()(Cs);return function(mt){return ut(P1(mt))}}())(l)),D=_(g)(function(ut){return ks(function(mt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:mt}}(ut))})(s),c=_(g)(function(ut){return ks(function(mt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:mt}}(ut))})(s),C=_(g)(function(ut){return ks(function(mt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:mt}}(ut))})(s);return[Fa(Ps(Es)(0)(v))(function(ut){return function(mt){return Pt(ft)(2)([He(ft)(0)(_(g)(En()(Mn))(C))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1e3,q:20})([ut])]),He(ft)(0)(_(g)(En()(Mn))(c))([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})([ut])]),He(ft)(0)(_(g)(En()(Mn))(D))([Bc(WD(Ct(gt()(J(J(kt)(qC)()()()({reflectSymbol:function(){return"q"}}))(FD)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:4e3,q:20})([ut])])])}})]})};return qr(p)([qr(p)([oe(ur)("tempo"),Eo(p)(Q_(fu)(h(f))(Y(T(k)))(Va(fu)(Z(Jo)(Co.value)("range"))(function(){return Va(fu)(Z(cf)(Si.value)("0"))(function(){return Va(fu)(Z(ff)(Ti.value)("100"))(function(){return Va(fu)(Z(_f)(Fi.value)("1"))(function(){return Va(X_)(Z(pf)(Oi.value)("50"))(function(){return Z(lf)(xi.value)(Yr(function(){var o=ve(f)(Kr)(Kf(Tn)(Gf)(n.slider)),m=Qn(ha)(Yc);return function(s){return o(m(ul(s)))}}()))})})})})})))([])]),gn(p)(sn(Gt)(h(f))(_(g)(function(){var o=Z(de)(me.value);return function(m){return o(Yr(x(m)))}}()))([Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(o){return o.value0})(e)))(Q(g)(u)(tt(rt))))(function(o){return function(){o();var s=ua(fe)(),l=hi(T(k))(at.create)(M1)(X0(s)(.91)(_(g)(wa(0)(.42)(100)(1.4))(a.slider))),v=jc(s)(i(l))(),D=X(nt)(v)(Cn(fe)(s));return t(X(nt)(D)(n.startStop.start(void 0)))(),n.startStop.stop(X(nt)(D)(Cn(fe)(s)))()}}),Qr(g)(a.startStop.stop)(function(o){return X(nt)(o)(X(nt)(t(S(f)(void 0)))(n.startStop.start(void 0)))})]))([un(ur)(he(Gt)(h(f))([Q(g)(u)("Turn on"),Q(g)(a.startStop.stop)("Turn off")]))])])}})))})}}};var R1=function(){return d.value}(),Nh=function(){return pr({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(p)(K(f))(d.value)(R1)({})}();var L1=function(){return d.value}(),Lh=function(){return pr({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(p)(K(f))(d.value)(L1)({})}();var U1=function(){return d.value}(),Bh=function(){return pr({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(p)(K(f))(d.value)(U1)({})}();var q1=function(){return d.value}(),Uh=function(t){return function(r){return function(e){return function(n){var a=function(i){return $a(I(f))(T(k))(n)(X(nt)(r(i))(ln))},u=Ma(t)(e);return pr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(B()(B()(B()(on()(B()(K(f))({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}})(p))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}})(p))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(p))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(p))(d.value)(q1)({next:a(L_.value),primer:L(Bh),inOcarina:L(Lh),flavors:L(Nh),ex0:L(Oh(u)(r)(n)),ex1:L(Ih(u)(r)(n)),ex2:L(Rh(u)(r)(n))})}}}};var z1=function(){return d.value}(),Wh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = bang $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(z1)({ai0:L(et(Tt(e)(t)(function(n){return Ao(Pn)(qt(ki)(qt(ki)(qt(ki)(_(Gc)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(wn(Pn)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(wn(Pn)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(wn(Pn)(Et(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(wn(Pn)(Et(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return Dt(n)([Pt(ft)(1)(function(){var u=function(i){return Y(T(k))(Vf()(Rf)(gs()(Br(ga)(i))(g_)))};return[jn(za)(a.tink0)(u(.1)),jn(za)(a.tink1)(u(.2)),jn(za)(a.tink2)(u(.9)),jn(za)(a.tink3)(u(1.8))]}())])}})))})}}};var V1=function(){return d.value}(),qh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = bang $ onOff $ dt (add n) apOn
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(V1)({ai0:L(et(Tt(e)(t)(function(n){return Ao(Pn)(qt(ki)(qt(ki)(qt(ki)(_(Gc)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(wn(Pn)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(wn(Pn)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(wn(Pn)(Et(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(wn(Pn)(Et(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return Dt(n)([Pt(ft)(1)(function(){var u=function(o){return Y(T(k))(Vf()(Rf)(gs()(Br(ga)(o))(g_)))},i=function(o){var m=su(xo)(o)(4);return m===0?a.tink0:m===1?a.tink1:m===2?a.tink2:a.tink3};return Qr(Or)(pn(0)(100))(function(o){var m=Gr(o);return jn(za)(i(o))(u(.3+.3*(m*Yi(1.005)(m))))})}())])}})))})}}};var j1=function(){return d.value}(),Hh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(j1)({ai0:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(lr(Cr)(a)(_t()))(function(u){return function(i){return Pt(ft)(.8)([kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:400,q:1})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:880,q:5})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:1200,q:10})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:2e3,q:20})([u]),kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var Q1=function(){return d.value}(),zh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(Q1)({ai0:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Fa(lr(Cr)(a)(_t()))(function(u){return function(i){return Pt(ft)(.8)(Qr(Or)(pn(0)(40))(Ka(Un)(Gr)(function(o){return kn(cn(Ct(gt()(J(J(kt)(An)()()()({reflectSymbol:function(){return"q"}}))(fn)()()()({reflectSymbol:function(){return"frequency"}})))(At()())))({frequency:200+o*150,q:30})([u])})))}})])}})))})}}};var Y1=function(){return d.value}(),Gh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(Y1)({ai0:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return Dt(n)([ru(function(u){return Pt(ft)(1)([jn(za)(a)(_t()),yo(tn)(.1)([Pt(ft)(.6)([u])])])})])}})))})}}};var tL=function(){return d.value}(),rL=function(t){return function(r){return Y(t)(En(r)(Mn)({p:[1,1,0],o:0,d:10}))}},eL=function(t){return function(r){return Y(t)(En(r)(Mn)({p:[1,1,0],o:0,d:8}))}},ol=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return yo(t)(n)([Pt(r)(a)([Bc(e)(u)(i)])])}}}}}}},Vh=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(p))(d.value)(tL)({txt:L(oe(ur)(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = bang
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
  ]`)),ai0:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return Dt(n)([Fa(jn(za)(a)(_t()))(function(u){return function(i){return ru(function(o){return Pt(ft)(1)([u,ol(tn)(ft)(tu)(.15)(.7)(1500)([ru(function(m){return He(ft)(1)(rL(T(k))())([ol(tn)(ft)(tu)(.4)(.5)(2500)([o,m])])})]),ol(tn)(ft)(tu)(.29)(.85)(2e3)([ru(function(m){return Pt(ft)(1)([ol(tn)(ft)(tu)(.6)(.6)(3500)([o,ru(function(s){return He(ft)(1)(eL(T(k))())([ol(tn)(ft)(tu)(.75)(.6)(4e3)([m,s]),ol(tn)(ft)(tu)(.75)(.55)(3e3)([u])])})])])})])])})}})])}})))})}}};var aL=function(){return d.value}(),Jh=function(t){return function(r){return function(e){return function(n){var a=function(u){return $a(I(f))(T(k))(n)(X(nt)(r(u))(ln))};return pr({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(p)(on()(K(f))({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})(p))(d.value)(aL)({hwLink:a(qf.value)})}}}};var oL=function(){return d.value}(),jh=function(t){return function(r){return function(e){return function(n){var a=function(i){return $a(I(f))(T(k))(n)(X(nt)(r(i))(ln))},u=Ma(t)(e);return pr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(B()(B()(B()(B()(B()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}})(p))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}})(p))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}})(p))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}})(p))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}})(p))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}})(p))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})(p))(d.value)(oL)({intro:L(Jh(t)(r)(e)(n)),next:a(R_.value),code0:L(Wh(u)(r)(n)),code1:L(qh(u)(r)(n)),code2:L(Hh(u)(r)(n)),code3:L(zh(u)(r)(n)),code4:L(Gh(u)(r)(n)),code5:L(Vh(u)(r)(n))})}}}};var Xh=function(t){return function(r){return function(e){return new P(j(t)("code")(r)(W(e)))}}},Pb=function(t){return Xh(t)(M(h(t.MonadST5().Monad0().Applicative0())))};var Qh=function(t){return function(r){return function(e){return new P(j(t)("pre")(r)(W(e)))}}},Ib=function(t){return Qh(t)(M(h(t.MonadST5().Monad0().Applicative0())))};var lL=function(){return d.value}(),Kh=function(t){return function(r){return function(e){return function(n){var a=X(nt)(r(N_.value))(ln),u=Ma(t)(e);return pr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(p)(B()(on()(B()(K(f))({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(p))(d.value)(lL)({code:L(Ib(p)([Pb(p)([oe(ur)(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:L(et(Tt(n)(u)(function(i){return S(ya)(void 0)})(function(i){return function(o){return Dt(i)([Pt(ft)(.15)([Wf(ef)(440)(_t())])])}}))),next:$a(I(f))(T(k))(n)(a)})}}}};var Yh=vc;var Zh=function(){return function(t){return t}};var tT=function(){return function(t){return t}};var Rb=function(){function t(){}return t.value=new t,t}();var rT={attr:function(t){return function(r){return b({key:"height",value:q(r)})}}};var Nb=function(){function t(){}return t.value=new t,t}();var eT={attr:function(t){return function(r){return b({key:"width",value:q(r)})}}};var Lb=function(t){return function(r){return function(e){return new P(j(t)("canvas")(r)(W(e)))}}};var Bb=function(){function t(){}return t.value=new t,t}(),Ub={attr:function(t){return function(r){return b({key:"@self@",value:ct(r)})}}};function om(t){return function(){return t.getContext("2d")}}function K_(t){return function(r){return function(){t.fillStyle=r}}}function im(t){return function(){t.beginPath()}}function fm(t){return function(){t.fill()}}function Wb(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function cm(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var xL=function(){return 2*Zi}(),il=function(t){return{o:t.value0+.04,n:t.value1,t:Wo}};var FL=function(){return d.value}(),fl=function(t){return function(r){return function(e){return function(n){return Y(t)(go(r)(Mn)({p:[e,n],o:0,d:16}))}}}},OL=function(t){return function(r){return Y(t)(En(r)(Mn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},$L=function(t){return function(r){return Y(t)(En(r)(Mn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var lm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return M_(t)(n)(a)([He(r)(u)(i)([jD(e)(o)(m)(s)])])}}}}}}}}}},nT=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(m){return function(s){return M_(t)(n)(a)([He(r)(u)(i)([JD(e)(o)(m)(s)])])}}}}}}}}}},ML=function(t){return function(r){return function(e){return function(n){return Y(t)(Zc(r)(Mn)({p:[e,n],o:0,d:16}))}}}},aT=400,qb=Gr(aT),wL=function(){return zt(Xa)(aT)+"px"}(),uT=600,Hb=Gr(uT),PL=function(){return zt(Xa)(uT)+"px"}(),IL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},oT=function(t){return function(r){return function(e){return pr({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))(d.value)(FL)({ex1:L(et(Ya()(k)(ue({reflectSymbol:function(){return"canvas"}})()()(ue({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"loading"}})()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(Y(T(k))(void 0))(a.startStop.start),i=function(o){return function(m){return function(s){var l=_(g)(function(v){return new at(v.acTime,v.value)})(rl(o)(a.slider));return[ws(Ms(Ct(gt()(J(J(kt)($s)()()()({reflectSymbol:function(){return"fftSize"}}))(Os)()()()({reflectSymbol:function(){return"cb"}})))(At()())))({cb:function(v){return function(){return Sn(new U(v))(s)(),Sn(V.value)(s)}},fftSize:ys.value})(S(pe)(Fa(jn(za)(m)(N(I(f))(_t())(_(g)(function(){var v=oa()(Be),D=Vn(bn)(wa(0)(.96)(100)(1.04));return function(c){return v(il(D(c)))}}())(l))))(function(v){return function(D){return ru(function(c){return Pt(ft)(1)([v,M_(qD(Ct(gt()(J(J(kt)(HC)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(OD)()()()({reflectSymbol:function(){return"delayTime"}})))(At()())))({maxDelayTime:2.5,delayTime:1})(_(g)(function(){var C=Zc()(Be),ut=Vn(bn)(wa(0)(.5)(100)(2.45));return function(mt){return C(il(ut(mt)))}}())(l))([He(ft)(.4)(_(g)(function(){var C=En()(Be),ut=Vn(bn)(wa(0)(.6)(100)(.9));return function(mt){return C(il(ut(mt)))}}())(l))([v])]),lm(tn)(ft)(tu)(.15)(M(h(f)))(.7)(M(h(f)))(1500)(fl(T(k))()(1500)(3e3))([ru(function(C){return He(ft)(1)(OL(T(k))())([lm(tn)(ft)(tu)(.4)(M(h(f)))(.5)(M(h(f)))(3e3)(fl(T(k))()(3e3)(100))([c,C])])})]),lm(tn)(ft)(tu)(.29)(_(g)(function(){var C=Zc()(Be),ut=Vn(bn)(wa(0)(.1)(100)(.4));return function(mt){return C(il(ut(mt)))}}())(l))(.85)(M(h(f)))(2e3)(fl(T(k))()(2e3)(5e3))([ru(function(C){return Pt(ft)(1)([lm(tn)(ft)(tu)(.6)(_(g)(function(){var ut=Zc()(Be),mt=Vn(bn)(wa(0)(.8)(100)(.3));return function(vr){return ut(il(mt(vr)))}}())(l))(.6)(M(h(f)))(3500)(fl(T(k))()(3500)(100))([c,ru(function(ut){return He(ft)(1)($L(T(k))())([nT(tn)(ft)(HD)(.75)(_(g)(function(){var mt=Zc()(Be),vr=Vn(bn)(wa(0)(.9)(100)(.1));return function(nr){return mt(il(vr(nr)))}}())(l))(.6)(M(h(f)))(4e3)(fl(T(k))()(4e3)(200))([C,ut]),nT(tn)(ft)(HD)(.75)(ML(T(k))()(.75)(.2))(.55)(M(h(f)))(200)(fl(T(k))()(200)(4e3))([v])])})])])})])])})}})))]}}};return qr(p)([Lb(p)(N(I(f))(sn(Gt)(h(f))(Y(T(k)))([Z(eT)(Nb.value)(PL),Z(rT)(Rb.value)(wL),Z(Tk)(Vt.value)("width: 100%;"),Z(Ub)(Bb.value)(function(){var o=ve(f)(Kr)(function(m){return function(){var l=om(m)();return K_(l)("black")(),cm(l)({width:Hb,height:qb,x:0,y:0})(),void 0}});return function(m){return o(pb(m))}}())]))(_(g)(function(o){return Z(Ub)(Bb.value)(function(){var m=ve(f)(Kr)(function(s){return function(){var v=om(s)();return K_(v)("black")(),cm(v)({width:Hb,height:qb,x:0,y:0})(),K_(v)("rgba(255,255,255,0.2)")(),Al(o)(function(D){return function(){return im(v)(),Wb(v)({end:xL,radius:D.value1*40,start:0,x:D.value0.x*Hb,y:D.value0.y*qb,useCounterClockwise:!1})(),fm(v)()}})()}});return function(s){return m(pb(s))}}())})(a.canvas)))([]),Eo(p)(sn(Gt)(h(f))(Y(T(k)))([Z(Jo)(Co.value)("range"),Z(cf)(Si.value)("0"),Z(ff)(Ti.value)("100"),Z(_f)(Fi.value)("1"),Z(pf)(Oi.value)("50"),Z(hk)(Vt.value)("width: 100%;"),Z(lf)(xi.value)(Yr(function(){var o=ve(f)(Kr)(Kf(Tn)(Gf)(n.slider)),m=Qn(ha)(Yc);return function(s){return o(m(ul(s)))}}()))]))([]),gn(p)(he(Gt)(h(f))([Y(T(k))(Z(Dc)(Vt.value)("width:100%; padding:1.0rem;")),sn(Gt)(h(f))(_(g)(function(){var o=Z(de)(me.value);return function(m){return o(Yr(x(m)))}}()))([Q(g)(a.startStop.loading)(S(f)(void 0)),Qr(g)(a.startStop.stop)(function(o){return X(nt)(o)(X(nt)(t(S(f)(void 0)))(n.startStop.start(void 0)))}),Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(o){return o.value0})(e)))(Q(g)(u)(tt(rt))))(function(o){return function(){o(),n.startStop.loading(void 0)();var s=se(V.value)(),l=ko(H(Le)(ua(Ne))(function(v){return H(Le)(zu(Ne)(v))(function(D){return H(Le)(_(Ai)(tT())(LE(Pn)(Yh)(Et(v))(Zh()(IL))))(function(c){return H(Le)(be(Ne)(e_(0)(5e4)))(function(C){var ut=hc(Zv(Ea(u_(c.pluck0))(gc(Av(kv()(c))))))({newSeed:kc(C),size:4});return be(Ne)(function(){var vr=Bn(ri)(f)(function(pt){return function(){var Tr=Uu(),ar=Uu();return{x:Tr,y:ar}}})(pn(0)(127))(),nr=Dt(v)(i(v)(ut)(s))(),Zr=Ot(af)(function(pt){return function(){var Tr=Re(s)();return _a(f)(Kr)(Tr)(function(ar){return function(){var ho=W_(ar)(),Gu=_(F)(function(){var $i=Ml(vr),cu=_(Or)(function(_n){return function(Xo){return Xo/255}(_n)});return function(_n){return $i(cu(_n))}}())(Gs(zs)(ho))();return n.canvas(Gu)(),void 0}})()}})(),le=X(nt)(X(nt)(X(nt)(nr)(D))(Cn(fe)(v)))(Zr);return n.startStop.stop(le)(),le})})})})}))();return t(function(){return n.startStop.start(void 0)(),zo(gi(l))()})(),void 0}})])]))([un(ur)(he(Gt)(h(f))([_(g)(x("Turn off"))(a.startStop.stop),_(g)(x("Turn on"))(u),_(g)(x("Loading..."))(a.startStop.loading)]))])])}})))})}}};var NL=function(){return d.value}(),iT=function(t){return function(r){return function(e){return function(n){var a=Ma(t)(e);return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(B()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})(p))(p)(NL)({next:$a(I(f))(T(k))(n)(X(nt)(r(qf.value))(ln)),ex:L(oT(a)(r)(n))})}}}};var BL=function(){return d.value}(),fT=function(t){return function(r){return function(e){return function(n){return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))(p)(BL)({next:Y(T(k))(Z(de)(me.value)(Yr(x(X(nt)(r(Rs.value))(ln)))))})}}}};var WL=function(){return d.value}(),cT=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
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
`}})()()(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(p))(p)(WL)({txt:L(oe(ur)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ bang
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`)),cancel:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Pt(ft)(1)([lr(Cr)(a)(he(Gt)(h(f))([_t(),Ru(1e3)($t(f)(oa()(Mn)({p:Ja(Zo)(Q(Or)(pn(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Ru(3e3)($t(f)(oa()(FC)({o:3.5})))]))])])}})))})}}};var HL=function(){return d.value}(),lT=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(p))(p)(HL)({txt:L(oe(ur)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ bang
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`)),envelope:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Pt(ft)(1)([lr(Cr)(a)(he(Gt)(h(f))([_t(),Ru(1e3)($t(f)(oa()(Mn)({p:Ja(Zo)(Q(Or)(pn(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})))})}}};var GL=function(){return d.value}(),_T=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})(p))(d.value)(GL)({numericEx:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Pt(ft)(1)([lr(Cr)(a)(jo(I(f))(_t())(function(){return jo(I(f))(Ru(1e3)(jo(I(f))($t(f)(oa()(Be)({n:1,o:1,t:hD})))(function(){return $t(f)(oa()(Be)({n:1.3,o:2,t:Wo}))})))(function(){return Ru(2500)(jo(I(f))($t(f)(oa()(Be)({n:1,o:2.5,t:hD})))(function(){return $t(f)(oa()(Be)({n:.7,o:3.5,t:xC}))}))})}))])])}})))})}}};var JL=function(){return d.value}(),pT=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ bang
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(p)(B()(K(f))({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})(p))(d.value)(JL)({suddenEx:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([Pt(ft)(1)([lr(Cr)(a)(he(Gt)(h(f))([_t(),Ru(1500)($t(f)(oa()(TC)({n:1.4})))]))])])}})))})}}};var XL=function(){return d.value}(),sT=function(t){return function(r){return function(e){return Mt({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      bang
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
`}})()()(B()(K(f))({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})(p))(p)(XL)({unitEx:L(et(Tt(e)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return Dt(n)([lr(Cr)(a)(he(Gt)(h(f))([_t(),$t(f)(oa()(CC(mi)(mi))(gC(Pt(ft)(1)([Is(hs)(1)(_t()),Pt(ft)(.2)([Uc(Ss)(100)([O_(Oc)(50)(_t())])])]))))]))])}})))})}}};var KL=function(){return d.value}(),mT=function(t){return function(r){return function(e){return function(n){var a=X(nt)(r(B_.value))(ln),u=Ma(t)(e);return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(B()(B()(on()(B()(B()(B()(K(f))({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}})(p))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}})(p))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}})(p))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(p))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(p))(p)(KL)({sudden:L(pT(u)(r)(n)),numeric:L(_T(u)(r)(n)),envelope:L(lT(u)(r)(n)),cancel:L(cT(u)(r)(n)),unit:L(sT(u)(r)(n)),next:$a(I(f))(T(k))(n)(a)})}}}};var ZL=function(){return d.value}(),vT=function(t){return function(r){return function(e){return function(n){return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))(p)(ZL)({next:Y(T(k))(Z(de)(me.value)(Yr(x(X(nt)(r(Ns.value))(ln)))))})}}}};var rB=function(){return d.value}(),DT=function(t){return function(r){return function(e){return function(n){return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(on()(K(f))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(p))(p)(rB)({next:Y(T(k))(Z(de)(me.value)(Yr(x(X(nt)(r(Hf.value))(ln)))))})}}}};var nB=function(){return d.value}(),dT=function(t){return function(r){return function(e){return function(n){return pr({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(p)(K(f))(d.value)(nB)({})}}}};var uB=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)
import FRP.Event.Class (bang)
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
        startE = bang unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ dyn $ map
                  ( \\i ->
                      OneOf.do
                        bang $ sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ bang $ silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap bang O.do
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
`,oB=iu(function(t){return Bt(function(r){return Ot(t)(function(e){return function(){var a=Uu();return r(e(a))()}})})}),iB=function(){return d.value}(),fB="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",bT=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(p)(B()(B()(K(f))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(p))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(p))(d.value)(iB)({txt:L(oe(ur)(uB)),ex1:L(et(Ya()(k)(ue({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ue({reflectSymbol:function(){return"loading"}})()()(ue({reflectSymbol:function(){return"start"}})()()(ue({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())(d.value)(function(n){return function(a){var u=N(I(f))(Y(T(k))(void 0))(a.startStop.start),i=hi(T(k))(at.create)(oB)(Iu(T(k))(function(m){return function(s){return s+1|0}})(a.slider)(0)),o=function(m){return[Pt(ft)(1)([Ip(_(g)(function(s){return he(Gt)(h(f))([Y(T(k))(AC(jn(S_(Ct(gt()(J(J(kt)(RC)()()()({reflectSymbol:function(){return"playbackRate"}}))(E_)()()()({reflectSymbol:function(){return"buffer"}})))(At()())))({buffer:m,playbackRate:.7+Ua(s)*2})(_t()))),Ru(5e3)(Y(T(k))(kC))])})(i))])]};return qr(p)([qr(p)([oe(ur)("Slide me!"),Eo(p)(sn(Gt)(h(f))(Y(T(k)))([Z(Jo)(Co.value)("range"),Z(cf)(Si.value)("0"),Z(ff)(Ti.value)("100"),Z(_f)(Fi.value)("1"),Z(pf)(Oi.value)("50"),Z(lf)(xi.value)(Yr(x(n.slider(void 0))))]))([])]),gn(p)(sn(Gt)(h(f))(_(g)(function(){var m=Z(de)(me.value);return function(s){return m(Yr(x(s)))}}()))([Q(g)(a.startStop.loading)(S(f)(void 0)),Qr(g)(a.startStop.stop)(function(m){return X(nt)(m)(X(nt)(t(S(f)(void 0)))(n.startStop.start(void 0)))}),Qr(g)(On(T(k))(N(I(f))(Y(T(k))(S(f)(void 0)))(_(g)(function(m){return m.value0})(e)))(Q(g)(u)(tt(rt))))(function(m){return function(){m(),n.startStop.loading(void 0)();var l=ko(H(Le)(ua(Ne))(function(v){return H(Le)(zu(Ne)(v))(function(D){return H(Le)(Et(v)(fB))(function(c){return be(Ne)(function(){var ut=Hs(o(c))(),mt=X(nt)(X(nt)(ut)(D))(Cn(fe)(v));return n.startStop.stop(mt)(),mt})})})}))();return t(function(){return n.startStop.start(void 0)(),zo(gi(l))()})(),void 0}})]))([un(ur)(he(Gt)(h(f))([_(g)(x("Turn off"))(a.startStop.stop),_(g)(x("Turn on"))(u)]))])])}})))})}}};var lB=function(){return d.value}(),yT=function(t){return function(r){return function(e){return function(n){var a=Ma(t)(e);return Mt({reflectType:function(){return`<div>
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
</div>`}})()()(B()(B()(K(f))({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}})(p))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})(p))(p)(lB)({appl:L(et(qs("\u{1F44F}")(n)(a)(function(u){return Et(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(i){return Dt(u)([Pt(ft)(1)([lr(Cr)(i)(_t())])])}}))),suby:L(bT(a)(r)(n))})}}}};var OLt=function(t){return t},$Lt={Coercible0:function(){}},pB=function(t){return function(r){var e=function(a){var u=function(i){if(i instanceof I_)return qr(p)(S(pe)(et(We(k)(iT(a.setCancellation)(a.setPage)))));if(i instanceof qf)return qr(p)(S(pe)(et(We(k)(Kh(a.setCancellation)(a.setPage)))));if(i instanceof N_)return qr(p)(S(pe)(et(We(k)(jh(a.setCancellation)(a.setPage)))));if(i instanceof R_)return qr(p)(S(pe)(et(We(k)(V0(a.setCancellation)(a.setPage)))));if(i instanceof Rs)return qr(p)(S(pe)(et(We(k)(DT(a.setCancellation)(a.setPage)))));if(i instanceof Hf)return qr(p)(S(pe)(et(We(k)(Uh(a.setCancellation)(a.setPage)))));if(i instanceof L_)return qr(p)(S(pe)(et(We(k)(mT(a.setCancellation)(a.setPage)))));if(i instanceof B_)return qr(p)(S(pe)(et(We(k)(xh(a.setCancellation)(a.setPage)))));if(i instanceof Ns)return qr(p)(S(pe)(et(We(k)(dT(a.setCancellation)(a.setPage)))));if(i instanceof KE)return qr(p)(S(pe)(et(We(k)(fT(a.setCancellation)(a.setPage)))));if(i instanceof U_)return qr(p)(S(pe)(et(We(k)(yT(a.setCancellation)(a.setPage)))));if(i instanceof YE)return qr(p)(S(pe)(et(We(k)(vT(a.setCancellation)(a.setPage)))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 144, column 5 - line 144, column 80): "+[i.constructor.name])};return u(a.page)},n=Iu(T(k))(function(a){if(a instanceof Vc)return function(u){return{prevPage:new U(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof YD)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 134, column 7 - line 136, column 75): "+[a.constructor.name])})(r)({prevPage:V.value,curPage:I_.value,cancel:S(f)(void 0),pageChange:!0});return[qr(p)(_(Or)(function(a){return Mv(p)([$v(p)(N(I(f))(sn(Gt)(h(f))(Y(T(k)))([Z(de)(me.value)(Yr(x(t(new Vc(a.value0))))),Z(Sk)(Vt.value)("cursor:pointer;")]))(_(g)(function(u){return Z(de)(me.value)(Yr(x(function(){return u.cancel(),t(new Vc(a.value0))()})))})(Bl(bu(f))(function(){var u=Du(La);return function(i){return u(function(o){return o.pageChange}(i))}}())(n))))([oe(ur)(a.value1.value0)]),bc(p)(Y(T(k))(Z(Hp)(Vt.value)(function(){return a.value1.value1?"":"display:none;"}())))([oe(ur)(" | ")])])})([new at(I_.value,new at("Home",!0)),new at(qf.value,new at("Hello world",!0)),new at(N_.value,new at("Array, fan, and fix",!0)),new at(R_.value,new at("Audio units",!0)),new at(Hf.value,new at("Events",!0)),new at(L_.value,new at("Parameters",!0)),new at(B_.value,new at("State",!0)),new at(U_.value,new at("Subgraphs",!1))])),qr(p)(S(pe)(Ev(k)(function(a){return e({page:a.curPage,setPage:function(u){return t(Vc.create(u))},setCancellation:function(u){return t(YD.create(u))}})})(Bl(bu(f))(function(a){return a.pageChange})(n))))]}},MLt=function(t){return{page:t,setPage:wt(Qe(ge(Ie))),setCancellation:wt(Qe(ge(Ie)))}},wLt=function(){var r=H(Tn)(H(Tn)(Ei)(mb))(p0)();return _a(f)(Kr)(_(Pe)(s0)(r))(function(e){return function(){var a=wv(),u=se(0)(),i=Gl(k)(k)(),o=Dk(p)(e)(pB(i.push)(i.event))(gg(u));return Ar(F)(Ot(o)(function(m){return m(a)}))(),i.push(new Vc(I_.value))()}})()};export{OLt as TopLevelSg,wLt as main,$Lt as newtypeTopLevelSg_,MLt as p2tl,pB as scene};
