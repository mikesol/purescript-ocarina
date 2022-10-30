var Rk=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var Zo={compose:function(t){return function(r){return function(e){return t(r(e))}}}},$u=function(t){return t.compose},Vl=function(t){var r=$u(t);return function(e){return function(n){return r(n)(e)}}};var ot=function(t){return t.identity},it={identity:function(t){return t},Semigroupoid0:function(){return Zo}};var Yr=!0;var lr=function(t){return function(r){return function(e){return t(e)(r)}}},g=function(t){return function(r){return t}};var Rf=function(t){return function(r){return r(t)}},gc=function(t){return function(r){return t(r)}};var D=function(){function t(){}return t.value=new t,t}();var A=function(t){return t.map},De=function(t){var r=A(t);return function(e){return function(n){return r(n)(e)}}},Wr=function(t){return A(t)(g(void 0))},j=function(t){var r=A(t);return function(e){return function(n){return r(g(n))(e)}}},ls=function(t){var r=A(t);return function(e){return r(g(e))}};var ya={map:$u(Zo)},Ar={map:Rk},uD=function(t){var r=A(t);return function(e){return function(n){return r(function(a){return a(n)})(e)}}};var Lk=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var se=function(t){return t.reflectSymbol};var Ni=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var ps=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Aa=function(t){return function(r){return r[t]}},wu=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var Bk={append:function(t){return function(r){return void 0}}};var rn={append:Lk};var vt=function(t){return t.append},oD=function(t){var r=vt(t);return{append:function(e){return function(n){return function(a){return r(e(a))(n(a))}}}}};var $t=function(t){return t.alt};var Wk=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,i=0;i<e;i++)for(var o=t[i],f=0;f<n;f++)a[u++]=o(r[f]);return a}};var Mw=ot(it);var Gl={apply:Wk,Functor0:function(){return Ar}},Dt=function(t){return t.apply};var or=function(t){var r=Dt(t),e=A(t.Functor0());return function(n){return function(a){return r(e(g(Mw))(n))(a)}}},cn=function(t){var r=Dt(t),e=A(t.Functor0());return function(n){return function(a){return function(u){return r(e(n)(a))(u)}}}};var C=function(t){return t.pure};var Nn=function(t){var r=C(t);return function(e){return function(n){if(e)return n;if(!e)return r(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,n.constructor.name])}}},jl=function(t){var r=Dt(t.Apply0()),e=C(t);return function(n){return function(a){return r(e(n))(a)}}};var Mu={pure:function(t){return[t]},Apply0:function(){return Gl}};var qk=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var Nw=ot(it);var _o={bind:qk,Apply0:function(){return Gl}},yt=function(t){return t.bind},xn=function(t){return lr(yt(t))};var hc=function(t){var r=yt(t);return function(e){return function(n){return function(a){return r(e(a))(n)}}}};var so=function(t){var r=yt(t);return function(e){return r(e)(Nw)}};var Ge=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},Rw=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},Lw=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},_s=typeof Array.prototype.fill=="function"?Rw:Lw,Bw=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],i=0,o=a;o!==r;)u[i++]=o.head,o=o.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),Rn=function(t){return t.length};var Uk=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var Hk=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var zk=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var Ww=function(){function t(r,e,n,a,u,i){var o,f,p,l,_,m,s;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),f=u,p=o,l=u;f<o&&p<i;)_=a[f],m=a[p],s=e(r(_)(m)),s>0?(n[l++]=m,++p):(n[l++]=_,++f);for(;f<o;)n[l++]=a[f++];for(;p<i;)n[l++]=a[p++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var Lf=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var Vk=function(t){return function(r){return t[r]}};var Uw=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var Jk={defer:function(t){return function(r){return t(void 0)(r)}}},Cc=function(t){return t.defer},cD=function(t){var r=Cc(t);return function(e){var n=Uw("go","Control.Lazy",function(){return r(function(u){return e(n(25))})}),a=n(25);return a}};var Na=function(t){var r=yt(t.Bind1()),e=C(t.Applicative0());return function(n){return function(a){return r(n)(function(u){return r(a)(function(i){return e(u(i))})})}}};var Hw=String.fromCharCode(65535),zw=String.fromCharCode(0),Vw=Number.POSITIVE_INFINITY,Jw=Number.NEGATIVE_INFINITY;var Gk=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var jk=Gk,Xk=Gk;var Qk=function(t){return function(r){return t===r}};var Kk=Qk,Yk=Qk;var Xl={eq:Yk},ti={eq:Kk};var Mt=function(t){return t.eq};var qt=function(){function t(){}return t.value=new t,t}(),rr=function(){function t(){}return t.value=new t,t}(),er=function(){function t(){}return t.value=new t,t}();var Zk=function(t){return function(r){return t-r|0}},tg=function(t){return function(r){return t-r}};var rg=function(t){return function(r){return t+r|0}},eg=function(t){return function(r){return t*r|0}},ng=function(t){return function(r){return t+r}},ag=function(t){return function(r){return t*r}};var Ln=function(t){return t.zero};var jn={add:ng,zero:0,mul:ag,one:1},Iu={add:rg,zero:0,mul:eg,one:1};var Xn=function(t){return t.one};var ln=function(t){return t.mul};var Rr=function(t){return t.add};var Ra=function(t){return t.sub};var Bf={sub:tg,Semiring0:function(){return jn}},lD={sub:Zk,Semiring0:function(){return Iu}};var Ql=function(t){var r=Ra(t),e=Ln(t.Semiring0());return function(n){return r(e)(n)}};var Pu=function(){return{compare:Xk(qt.value)(er.value)(rr.value),Eq0:function(){return Xl}}}(),Ie=function(){return{compare:jk(qt.value)(er.value)(rr.value),Eq0:function(){return ti}}}();var Ut=function(t){return t.compare};var og=function(t){var r=Ut(t);return function(e){return function(n){var a=r(e)(n);return!(a instanceof qt)}}};var Ri=function(t){var r=Ut(t);return function(e){return function(n){var a=r(e)(n);if(a instanceof qt)return n;if(a instanceof er||a instanceof rr)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[a.constructor.name])}}};var _D=function(t){var r=og(t);return function(e){var n=Ln(e.Semiring0()),a=Ql(e);return function(u){var i=r(u)(n);return i?u:a(u)}}};var Bn=function(t){return t.top};var Zl={top:2147483647,bottom:-2147483648,Ord0:function(){return Ie}};var Wn=function(t){return t.bottom};var fg=function(t){return t.toString()};var ga={show:fg};var Rt=function(t){return t.show};var pM=ot(it),F=function(){function t(){}return t.value=new t,t}(),h=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Ft=function(t){return function(r){return function(e){if(e instanceof F)return t;if(e instanceof h)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Te={map:function(t){return function(r){return r instanceof h?new h(t(r.value0)):F.value}}},_M=A(Te);var qn=function(t){return Ft(t)(pM)},Sn=function(){return function(t){if(t instanceof h)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Bi={apply:function(t){return function(r){if(t instanceof h)return _M(t.value0)(r);if(t instanceof F)return F.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Te}};var Un={bind:function(t){return function(r){if(t instanceof h)return r(t.value0);if(t instanceof F)return F.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Bi}};var ri=function(){return{pure:h.create,Apply0:function(){return Bi}}}();var Ht=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),zt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Uf={map:function(t){return function(r){if(r instanceof Ht)return new Ht(r.value0);if(r instanceof zt)return new zt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): "+[r.constructor.name])}}};var la=function(t){return function(r){return function(e){if(e instanceof Ht)return t(e.value0);if(e instanceof zt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},ss=function(){return la(g(F.value))(h.create)}();var La=function(t){return t};var Nu={map:function(t){return function(r){return t(r)}}};var cg={apply:function(t){return function(r){return t(r)}},Functor0:function(){return Nu}},sM={bind:function(t){return function(r){return r(t)}},Apply0:function(){return cg}},vD={pure:La,Apply0:function(){return cg}},iu={Applicative0:function(){return vD},Bind1:function(){return sM}};var lg=function(t){return Math.min(Math.abs(t),2147483647)},pg=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},_g=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},sg=function(t){return function(r){return t/r}};var vg={Ring0:function(){return Bf}},mg={Ring0:function(){return lD}};var ha=function(t){return t.mod};var vs={degree:function(t){return 1},div:sg,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return vg}},Ru={degree:lg,div:pg,mod:_g,CommutativeRing0:function(){return mg}},Lu=function(t){return t.div};var Sc={mempty:void 0,Semigroup0:function(){return Bk}};var pr=function(t){return t.mempty},mD=function(t){var r=pr(t),e=oD(t.Semigroup0());return{mempty:function(n){return r},Semigroup0:function(){return e}}};var DD=function(t){return function(){return t}},Dg=function(t){return function(r){return function(){return r(t())()}}};var np=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var dg=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},ei={Applicative0:function(){return Yt},Bind1:function(){return Kn}},Kn={bind:Dg,Apply0:function(){return dD(0)}},Yt={pure:DD,Apply0:function(){return dD(0)}},bg=dg("functorEffect","Effect",function(){return{map:jl(Yt)}}),dD=dg("applyEffect","Effect",function(){return{apply:Na(ei),Functor0:function(){return bg(0)}}}),R=bg(20),vr=dD(23),bM=cn(vr),bD=function(t){return{append:bM(vt(t))}},Hf=function(t){var r=bD(t.Semigroup0());return{mempty:DD(pr(t)),Semigroup0:function(){return r}}};var yg=function(t){return function(){return{value:t}}};var zr=function(t){return function(){return t.value}},Ag=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},te=function(t){return function(r){return function(){r.value=t}}};var AM=Wr(R),Nr=yg,kM=Ag,zf=function(t){return kM(function(r){var e=t(r);return{state:e,value:e}})},ni=function(t){return function(r){return AM(zf(t)(r))}};var kg=function(t){return function(r){return function(){return t(r())}}},gg=function(t){return function(){return t}},hg=function(t){return function(r){return function(){return r(t())()}}};function We(t){return function(){return{value:t}}}var pn=function(t){return function(){return t.value}},Cg=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},Wa=function(t){return function(r){return function(){return r.value=t}}};var SM=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},EM=Cg,qu=function(t){return EM(function(r){var e=t(r);return{state:e,value:e}})},pa={map:kg};var AD={Applicative0:function(){return je},Bind1:function(){return Ec}},Ec={bind:hg,Apply0:function(){return kD(0)}},je={pure:gg,Apply0:function(){return kD(0)}},kD=SM("applyST","Control.Monad.ST.Internal",function(){return{apply:Na(AD),Functor0:function(){return pa}}}),gD=kD(47);function Vf(){return[]}var hD=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var ds=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function FM(t){return function(){return t.slice()}}var bs=FM;var $M=function(){function t(r,e,n,a,u,i){var o,f,p,l,_,m,s;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),f=u,p=o,l=u;f<o&&p<i;)_=a[f],m=a[p],s=e(r(_)(m)),s>0?(n[l++]=m,++p):(n[l++]=_,++f);for(;f<o;)n[l++]=a[f++];for(;p<i;)n[l++]=a[p++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var Wi=function(t){return hD([t])};var $g=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},Og=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var w=function(t){return t.empty};var G=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),wc=function(t){return function(r){return t(r.value0)(r.value1)}};var qe=function(t){return t.value1};var vo={map:function(t){return function(r){return new G(r.value0,t(r.value1))}}};var _a=function(t){return t.value0};var Jf=function(t){return function(r){return function(e){return t(new G(r,e))}}};var et=function(t){return t};var M=function(){return et};var ED=M(),ui=function(){return ED};var he=function(){return ED};var Pg=function(){return function(){return function(t){return ED}}};var ue=function(t){return t.foldr};var me=function(t){var r=ue(t);return function(e){return r($t(e.Alt0()))(w(e))}},Hn=function(t){var r=ue(t);return function(e){var n=$t(e.Alt0()),a=w(e);return function(u){return r(function(i){return n(u(i))})(a)}}},dn=function(t){var r=or(t.Apply0()),e=C(t);return function(n){var a=ue(n);return function(u){return a(function(i){return r(u(i))})(e(void 0))}}},Ua=function(t){var r=dn(t);return function(e){return lr(r(e))}};var oe=function(t){return t.foldl};var Ue={foldr:function(t){return function(r){return function(e){if(e instanceof F)return r;if(e instanceof h)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof F)return r;if(e instanceof h)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){var r=pr(t);return function(e){return function(n){if(n instanceof F)return r;if(n instanceof h)return e(n.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,n.constructor.name])}}}};var Rg=function(t){var r=ue(t);return function(e){var n=vt(e.Semigroup0()),a=pr(e);return function(u){return r(function(i){return function(o){return n(u(i))(o)}})(a)}}},Gt={foldr:$g,foldl:Og,foldMap:function(t){return Rg(Gt)(t)}};var en=function(t){return t.foldMap};var Lg=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(i){return[a,u,i]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(i){return function(o){return function(f){function p(l,_){switch(_-l){case 0:return i([]);case 1:return u(t)(o(f[l]));case 2:return a(u(r)(o(f[l])))(o(f[l+1]));case 3:return a(a(u(e)(o(f[l])))(o(f[l+1])))(o(f[l+2]));default:var m=l+Math.floor((_-l)/4)*2;return a(u(n)(p(l,m)))(p(m,_))}}return p(0,f.length)}}}}}}();var EI=ot(it),bn=function(t){return t.traverse};var Xg=function(t){var r=bn(t);return function(e){return r(e)(EI)}},Do={traverse:function(t){var r=t.Apply0();return Lg(Dt(r))(A(r.Functor0()))(C(t))},sequence:function(t){return Xg(Do)(t)},Functor0:function(){return Ar},Foldable1:function(){return Gt}};var VI=Sn();var JI=vt(rn);var bp=function(){return Lf(G.create)}();var Nc=function(){return Vk};var rh=function(t){return[t]};var U=function(t){return function(r){return Lf(t)(Ge(0)(Rn(r)-1|0))(r)}};var yp=function(){return Uk(h.create)(F.value)}(),UD=function(t){return yp(t)(Rn(t)-1|0)};var eh=function(){return Hk(h.create)(F.value)}();var HD=function(){return zk(h.create)(F.value)}(),zD=function(t){return function(r){return function(e){return e.length===0?[]:Ft(e)(function(n){return VI(HD(n)(e))})(eh(t(r))(e))}}};var Hi=function(t){return function(r){return JI([t])(r)}};var nh=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var bo=function(t){return t.mapWithIndex};var ii={mapWithIndex:nh,Functor0:function(){return Ar}};var yo=function(t){return t.foldrWithIndex};var Hu=function(t){return t.foldlWithIndex};var fi=function(t){return t.foldMapWithIndex};var zi=function(t){return t.traverseWithIndex};var zu=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Fs=function(t){var r=w(t);return function(e){return new zu(e,r)}};var Ce=function(){function t(){}return t.value=new t,t}(),Zt=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),$s=function(t){return t},o1=function(t){return new Zt(t.value0,t.value1)};var i1=function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(f,p){if(p instanceof Zt&&p.value1 instanceof Zt&&p.value1.value1 instanceof Zt){a=new Zt(p,f),n=p.value1.value1.value1;return}var l=function(m){return m instanceof Zt&&m.value1 instanceof Zt&&m.value1.value1 instanceof Ce?new Zt(t(m.value0),new Zt(t(m.value1.value0),Ce.value)):m instanceof Zt&&m.value1 instanceof Ce?new Zt(t(m.value0),Ce.value):Ce.value},_=function(m){return function(s){var c=m,v=!1,K;function pt(xt,xr){if(xt instanceof Zt&&xt.value0 instanceof Zt&&xt.value0.value1 instanceof Zt&&xt.value0.value1.value1 instanceof Zt){c=xt.value1,s=new Zt(t(xt.value0.value0),new Zt(t(xt.value0.value1.value0),new Zt(t(xt.value0.value1.value1.value0),xr)));return}return v=!0,xr}for(;!v;)K=pt(c,s);return K}};return u=!0,_(f)(l(p))}for(;!u;)i=o(a,n);return i}};return r(Ce.value)},Ap={map:i1};var sa={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(i){var o=u,f=!1,p;function l(_,m){if(m instanceof Ce)return f=!0,_;if(m instanceof Zt){o=new Zt(m.value0,_),i=m.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[_.constructor.name,m.constructor.name])}for(;!f;)p=l(o,i);return p}};return a(Ce.value)}(),n=oe(sa)(lr(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(f,p){if(p instanceof Ce)return u=!0,f;if(p instanceof Zt){a=t(f)(p.value0),n=p.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[p.constructor.name])}for(;!u;)i=o(a,n);return i}};return r},foldMap:function(t){var r=vt(t.Semigroup0()),e=pr(t);return function(n){return oe(sa)(function(a){var u=r(a);return function(i){return u(n(i))}})(e)}}};var f1=ue(sa);var Os={append:function(t){return function(r){return f1(Zt.create)(r)(t)}}},uh=vt(Os);var GD={append:function(t){return function(r){return new zu(t.value0,uh(t.value1)(o1(r)))}}};var oh={alt:uh,Functor0:function(){return Ap}},jD=function(){return{empty:Ce.value,Alt0:function(){return oh}}}();var _h=function(t){return t()};var sh=function(t){throw new Error(t)};var vh=function(){return sh};var $1=vh(),O1=_h,za=function(t){return O1(function(){return $1(t)})};var Lt=function(){function t(){}return t.value=new t,t}(),ir=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Br=function(){function t(r,e,n,a,u,i,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i,this.value6=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return new t(r,e,n,a,u,i,o)}}}}}}},t}(),Vi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),li=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),Ji=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),ho=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Gi=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Ms=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var dh=function(t){return function(r){return new ir(Lt.value,t,r,Lt.value)}};var Ps=function(t){var r=Ut(t);return function(e){var n=function(a){var u=!1,i;function o(f){if(f instanceof Lt)return u=!0,F.value;if(f instanceof ir){var p=r(e)(f.value1);if(p instanceof er)return u=!0,new h(f.value2);if(p instanceof qt){a=f.value0;return}a=f.value3;return}if(f instanceof Br){var l=r(e)(f.value1);if(l instanceof er)return u=!0,new h(f.value2);var _=r(e)(f.value4);if(_ instanceof er)return u=!0,new h(f.value5);if(l instanceof qt){a=f.value0;return}if(_ instanceof rr){a=f.value6;return}a=f.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[f.constructor.name])}for(;!u;)i=o(a);return i};return n}};var bh=function(t){return t instanceof Lt};var yh=function(t){return function(r){return function(e){var n=t,a=r,u=!1,i;function o(f,p,l){if(p instanceof Ce)return u=!0,l;if(p instanceof Zt){if(p.value0 instanceof Vi){n=f,a=p.value1,e=new ir(l,p.value0.value0,p.value0.value1,p.value0.value2);return}if(p.value0 instanceof li){n=f,a=p.value1,e=new ir(p.value0.value0,p.value0.value1,p.value0.value2,l);return}if(p.value0 instanceof Ji){n=f,a=p.value1,e=new Br(l,p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof ho){n=f,a=p.value1,e=new Br(p.value0.value0,p.value0.value1,p.value0.value2,l,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof Gi){n=f,a=p.value1,e=new Br(p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5,l);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[p.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[p.constructor.name,l.constructor.name])}for(;!u;)i=o(n,a,e);return i}}},gp=function(t){var r=yh(t),e=Ut(t);return function(n){return function(a){var u=function(o){return function(f){var p=o,l=!1,_;function m(s,c){if(s instanceof Ce)return l=!0,new ir(c.value0,c.value1,c.value2,c.value3);if(s instanceof Zt){if(s.value0 instanceof Vi)return l=!0,r(s.value1)(new Br(c.value0,c.value1,c.value2,c.value3,s.value0.value0,s.value0.value1,s.value0.value2));if(s.value0 instanceof li)return l=!0,r(s.value1)(new Br(s.value0.value0,s.value0.value1,s.value0.value2,c.value0,c.value1,c.value2,c.value3));if(s.value0 instanceof Ji){p=s.value1,f=new Ms(new ir(c.value0,c.value1,c.value2,c.value3),s.value0.value0,s.value0.value1,new ir(s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5));return}if(s.value0 instanceof ho){p=s.value1,f=new Ms(new ir(s.value0.value0,s.value0.value1,s.value0.value2,c.value0),c.value1,c.value2,new ir(c.value3,s.value0.value3,s.value0.value4,s.value0.value5));return}if(s.value0 instanceof Gi){p=s.value1,f=new Ms(new ir(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3),s.value0.value4,s.value0.value5,new ir(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[s.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[s.constructor.name,c.constructor.name])}for(;!l;)_=m(p,f);return _}},i=function(o){return function(f){var p=o,l=!1,_;function m(s,c){if(c instanceof Lt)return l=!0,u(s)(new Ms(Lt.value,n,a,Lt.value));if(c instanceof ir){var v=e(n)(c.value1);if(v instanceof er)return l=!0,r(s)(new ir(c.value0,n,a,c.value3));if(v instanceof qt){p=new Zt(new Vi(c.value1,c.value2,c.value3),s),f=c.value0;return}p=new Zt(new li(c.value0,c.value1,c.value2),s),f=c.value3;return}if(c instanceof Br){var v=e(n)(c.value1);if(v instanceof er)return l=!0,r(s)(new Br(c.value0,n,a,c.value3,c.value4,c.value5,c.value6));var K=e(n)(c.value4);if(K instanceof er)return l=!0,r(s)(new Br(c.value0,c.value1,c.value2,c.value3,n,a,c.value6));if(v instanceof qt){p=new Zt(new Ji(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),s),f=c.value0;return}if(v instanceof rr&&K instanceof qt){p=new Zt(new ho(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),s),f=c.value3;return}p=new Zt(new Gi(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),s),f=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[s.constructor.name,c.constructor.name])}for(;!l;)_=m(p,f);return _}};return i(Ce.value)}}},N1=function(t){var r=yh(t),e=Ut(t);return function(n){var a=function(f){return function(p){var l=f,_=!1,m;function s(c,v){if(c instanceof Ce)return _=!0,v;if(c instanceof Zt){if(c.value0 instanceof Vi&&c.value0.value2 instanceof Lt&&v instanceof Lt)return _=!0,r(c.value1)(new ir(Lt.value,c.value0.value0,c.value0.value1,Lt.value));if(c.value0 instanceof li&&c.value0.value0 instanceof Lt&&v instanceof Lt)return _=!0,r(c.value1)(new ir(Lt.value,c.value0.value1,c.value0.value2,Lt.value));if(c.value0 instanceof Vi&&c.value0.value2 instanceof ir){l=c.value1,p=new Br(v,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof li&&c.value0.value0 instanceof ir){l=c.value1,p=new Br(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,v);return}return c.value0 instanceof Vi&&c.value0.value2 instanceof Br?(_=!0,r(c.value1)(new ir(new ir(v,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new ir(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof li&&c.value0.value0 instanceof Br?(_=!0,r(c.value1)(new ir(new ir(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new ir(c.value0.value0.value6,c.value0.value1,c.value0.value2,v)))):c.value0 instanceof Ji&&c.value0.value2 instanceof Lt&&c.value0.value5 instanceof Lt&&v instanceof Lt?(_=!0,r(c.value1)(new Br(Lt.value,c.value0.value0,c.value0.value1,Lt.value,c.value0.value3,c.value0.value4,Lt.value))):c.value0 instanceof ho&&c.value0.value0 instanceof Lt&&c.value0.value5 instanceof Lt&&v instanceof Lt?(_=!0,r(c.value1)(new Br(Lt.value,c.value0.value1,c.value0.value2,Lt.value,c.value0.value3,c.value0.value4,Lt.value))):c.value0 instanceof Gi&&c.value0.value0 instanceof Lt&&c.value0.value3 instanceof Lt&&v instanceof Lt?(_=!0,r(c.value1)(new Br(Lt.value,c.value0.value1,c.value0.value2,Lt.value,c.value0.value4,c.value0.value5,Lt.value))):c.value0 instanceof Ji&&c.value0.value2 instanceof ir?(_=!0,r(c.value1)(new ir(new Br(v,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof ho&&c.value0.value0 instanceof ir?(_=!0,r(c.value1)(new ir(new Br(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,v),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof ho&&c.value0.value5 instanceof ir?(_=!0,r(c.value1)(new ir(c.value0.value0,c.value0.value1,c.value0.value2,new Br(v,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof Gi&&c.value0.value3 instanceof ir?(_=!0,r(c.value1)(new ir(c.value0.value0,c.value0.value1,c.value0.value2,new Br(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,v)))):c.value0 instanceof Ji&&c.value0.value2 instanceof Br?(_=!0,r(c.value1)(new Br(new ir(v,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new ir(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof ho&&c.value0.value0 instanceof Br?(_=!0,r(c.value1)(new Br(new ir(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new ir(c.value0.value0.value6,c.value0.value1,c.value0.value2,v),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof ho&&c.value0.value5 instanceof Br?(_=!0,r(c.value1)(new Br(c.value0.value0,c.value0.value1,c.value0.value2,new ir(v,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new ir(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof Gi&&c.value0.value3 instanceof Br?(_=!0,r(c.value1)(new Br(c.value0.value0,c.value0.value1,c.value0.value2,new ir(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new ir(c.value0.value3.value6,c.value0.value4,c.value0.value5,v)))):(_=!0,za("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!_;)m=s(l,p);return m}},u=function(f){return function(p){var l=f,_=!1,m;function s(c,v){if(v instanceof ir&&v.value0 instanceof Lt&&v.value3 instanceof Lt)return _=!0,a(c)(Lt.value);if(v instanceof ir){l=new Zt(new li(v.value0,v.value1,v.value2),c),p=v.value3;return}if(v instanceof Br&&v.value0 instanceof Lt&&v.value3 instanceof Lt&&v.value6 instanceof Lt)return _=!0,a(new Zt(new li(Lt.value,v.value1,v.value2),c))(Lt.value);if(v instanceof Br){l=new Zt(new Gi(v.value0,v.value1,v.value2,v.value3,v.value4,v.value5),c),p=v.value6;return}return _=!0,za("The impossible happened in partial function `removeMaxNode`.")}for(;!_;)m=s(l,p);return m}},i=function(f){var p=!1,l;function _(m){if(m instanceof ir&&m.value3 instanceof Lt)return p=!0,{key:m.value1,value:m.value2};if(m instanceof ir){f=m.value3;return}if(m instanceof Br&&m.value6 instanceof Lt)return p=!0,{key:m.value4,value:m.value5};if(m instanceof Br){f=m.value6;return}return p=!0,za("The impossible happened in partial function `maxNode`.")}for(;!p;)l=_(f);return l},o=function(f){return function(p){var l=f,_=!1,m;function s(c,v){if(v instanceof Lt)return _=!0,F.value;if(v instanceof ir){var K=e(n)(v.value1);if(v.value3 instanceof Lt&&K instanceof er)return _=!0,new h(new G(v.value2,a(c)(Lt.value)));if(K instanceof er){var pt=i(v.value0);return _=!0,new h(new G(v.value2,u(new Zt(new Vi(pt.key,pt.value,v.value3),c))(v.value0)))}if(K instanceof qt){l=new Zt(new Vi(v.value1,v.value2,v.value3),c),p=v.value0;return}l=new Zt(new li(v.value0,v.value1,v.value2),c),p=v.value3;return}if(v instanceof Br){var xt=function(){return v.value0 instanceof Lt&&v.value3 instanceof Lt&&v.value6 instanceof Lt}(),K=e(n)(v.value4),xr=e(n)(v.value1);if(xt&&xr instanceof er)return _=!0,new h(new G(v.value2,r(c)(new ir(Lt.value,v.value4,v.value5,Lt.value))));if(xt&&K instanceof er)return _=!0,new h(new G(v.value5,r(c)(new ir(Lt.value,v.value1,v.value2,Lt.value))));if(xr instanceof er){var pt=i(v.value0);return _=!0,new h(new G(v.value2,u(new Zt(new Ji(pt.key,pt.value,v.value3,v.value4,v.value5,v.value6),c))(v.value0)))}if(K instanceof er){var pt=i(v.value3);return _=!0,new h(new G(v.value5,u(new Zt(new ho(v.value0,v.value1,v.value2,pt.key,pt.value,v.value6),c))(v.value3)))}if(xr instanceof qt){l=new Zt(new Ji(v.value1,v.value2,v.value3,v.value4,v.value5,v.value6),c),p=v.value0;return}if(xr instanceof rr&&K instanceof qt){l=new Zt(new ho(v.value0,v.value1,v.value2,v.value4,v.value5,v.value6),c),p=v.value3;return}l=new Zt(new Gi(v.value0,v.value1,v.value2,v.value3,v.value4,v.value5),c),p=v.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[v.constructor.name])}for(;!_;)m=s(l,p);return m}};return o(Ce.value)}},zn={foldr:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof ir)return ue(zn)(t)(t(e.value2)(ue(zn)(t)(r)(e.value3)))(e.value0);if(e instanceof Br)return ue(zn)(t)(t(e.value2)(ue(zn)(t)(t(e.value5)(ue(zn)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof ir)return oe(zn)(t)(t(oe(zn)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Br)return oe(zn)(t)(t(oe(zn)(t)(t(oe(zn)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){var r=pr(t),e=vt(t.Semigroup0());return function(n){return function(a){if(a instanceof Lt)return r;if(a instanceof ir)return e(en(zn)(t)(n)(a.value0))(e(n(a.value2))(en(zn)(t)(n)(a.value3)));if(a instanceof Br)return e(en(zn)(t)(n)(a.value0))(e(n(a.value2))(e(en(zn)(t)(n)(a.value3))(e(n(a.value5))(en(zn)(t)(n)(a.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[a.constructor.name])}}}},Vn={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof ir)return yo(Vn)(t)(t(e.value1)(e.value2)(yo(Vn)(t)(r)(e.value3)))(e.value0);if(e instanceof Br)return yo(Vn)(t)(t(e.value1)(e.value2)(yo(Vn)(t)(t(e.value4)(e.value5)(yo(Vn)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof Lt)return r;if(e instanceof ir)return Hu(Vn)(t)(t(e.value1)(Hu(Vn)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Br)return Hu(Vn)(t)(t(e.value4)(Hu(Vn)(t)(t(e.value1)(Hu(Vn)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){var r=pr(t),e=vt(t.Semigroup0());return function(n){return function(a){if(a instanceof Lt)return r;if(a instanceof ir)return e(fi(Vn)(t)(n)(a.value0))(e(n(a.value1)(a.value2))(fi(Vn)(t)(n)(a.value3)));if(a instanceof Br)return e(fi(Vn)(t)(n)(a.value0))(e(n(a.value1)(a.value2))(e(fi(Vn)(t)(n)(a.value3))(e(n(a.value4)(a.value5))(fi(Vn)(t)(n)(a.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[a.constructor.name])}}},Foldable0:function(){return zn}},R1=yo(Vn),L1=Hu(Vn),Ah=function(){return R1(function(t){return function(r){return function(e){return new Zt(t,e)}}})(Ce.value)}();var ji=function(){return Lt.value}();var ZD=function(t){var r=N1(t);return function(e){return function(n){return Ft(n)(qe)(r(e)(n))}}};var Kf=function(t){var r=Ps(t),e=ZD(t),n=gp(t);return function(a){return function(u){return function(i){var o=a(r(u)(i));if(o instanceof F)return e(u)(i);if(o instanceof h)return n(u)(o.value0)(i);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[o.constructor.name])}}}};var B1=function(t){var r=Kf(t);return function(e){return function(n){return function(a){var u=function(i){return function(o){return function(f){return r(function(){var p=Ft(f)(e(f));return function(l){return h.create(p(l))}}())(i)(o)}}};return L1(u)(a)(n)}}}};var kh=function(t){return B1(t)(g)};var Ns=function(t){return t.partitionMap};var pi=function(t){return t.filterMap};var xp=function(t){return t.filter};var Rs=(t,r)=>{for(var e=0,n=t.length;e<n;e++)r(t[e])},td=(t,r)=>{for(let e in t)r(t[e])},rd=()=>({}),ed=(t,r,e)=>{e[t]=r},nd=(t,r)=>{delete r[t]};var xh=et;var Vu={liftST:xh,Monad0:function(){return ei}},Ju=function(t){return t.liftST};var j1=en(sa),X1=oe(sa),Q1=ue(sa);var K1=function(t){var r=kh(t);return function(e){return function(n){return r(e)(n)}}};var ad=function(t){return Ah(t)};var Eh=function(t){return dh(t)(void 0)};var ud=function(t){return{append:K1(t)}};var Fh=function(t){return bh(t)},$h=function(t){var r=gp(t);return function(e){return function(n){return r(e)(void 0)(n)}}};var Oh={foldMap:function(t){var r=j1(t);return function(e){var n=r(e);return function(a){return n(ad(a))}}},foldl:function(t){return function(r){var e=X1(t)(r);return function(n){return e(ad(n))}}},foldr:function(t){return function(r){var e=Q1(t)(r);return function(n){return e(ad(n))}}}};var od=ji;var wh=function(t){var r=ud(t);return{mempty:od,Semigroup0:function(){return r}}};var Ls=function(t){var r=ZD(t);return function(e){return function(n){return r(e)(n)}}};function Mh(t){return function(r){return function(){return setTimeout(r,t)}}}function Ih(t){return function(){clearTimeout(t)}}var Z1=Ut(Ie);var Bs=Mh;var tP={eq:function(t){return function(r){return t===r}}},Ws={compare:function(t){return function(r){return Z1(t)(r)}},Eq0:function(){return tP}};var Sp=Ih;var Qi=function(r){return function(e){return r(e)()}};var Wc=function(r){return function(e){return function(){return r(e)}}};var dP=function(t){var r=vt(bD(t));return{append:function(e){return function(n){return Qi(function(a){return r(Wc(e)(a))(Wc(n)(a))})}}}};var Lh=function(t){var r=pr(Hf(t)),e=dP(t.Semigroup0());return{mempty:Qi(function(n){return r}),Semigroup0:function(){return e}}};var bP=ot(it),yP=A(vo),AP=C(ri),_i=function(t){return t.sampleOnRight};var Co=function(t){return t.keepLatest};var qc=function(t){return t.fix},fu=function(t){var r=qc(t),e=_i(t),n=t.Alternative0(),a=$t(n.Plus1().Alt0()),u=C(n.Applicative0()),i=A(t.Filterable1().Functor1());return function(o){return function(f){return function(p){return r(function(l){return e(a(l)(u(f)))(i(lr(o))(p))})}}}};var Uc=function(t){var r=pi(t.Filterable1()),e=fu(t);return function(n){return function(a){return function(u){return r(qe)(e(function(i){return function(o){return yP(AP)(n(i.value0)(o))}})(new G(a,F.value))(u))}}}},Us=function(t){var r=pi(t.Filterable1()),e=fu(t);return function(n){var a=function(u){return function(i){if(u instanceof F)return new h({now:i,last:F.value});if(u instanceof h)return new h({now:i,last:new h(u.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 83, column 3 - line 83, column 50): "+[u.constructor.name,i.constructor.name])}};return r(bP)(e(a)(F.value)(n))}};function fd(t){return function(r){return t===r}}var cd=fd;var Wh=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},qh=Ua(Yt),Hs=qh(Ue),Zf=C(Yt),Uh=pr(Lh(Sc)),Ki=Ju(Vu),Ep=Wr(R),hP=vt(rn),CP=pr(wh(Ws)),xP=Ls(Ws),TP=vt(ud(Ws)),SP=qh(Oh),EP=Dt(vr),FP=A(R);var $P=function(t){return function(r){return function(e,n){var a=Nr(F.value)(),u=t(e,function(o){return te(new h(o))(a)()}),i=r(e,function(o){var f=zr(a)();return Hs(f)(function(p){return function(){return n(o(p))}})()});return function(){return u(),i()}}}},OP=function(t){return function(r){return function(e,n){var a=Nr(F.value)(),u=t(e,function(o){var f=zr(a)();return Hs(f)(function(p){return function(){return n(p(o))}})()}),i=r(e,function(o){return te(new h(o))(a)()});return function(){return u(),i()}}}},wP=function(t){return function(r,e){var n=Nr(Zf(void 0))(),a=t(r,function(u){var i=zr(n)();i();var o=u(r,e);return te(o)(n)()});return function(){var i=zr(n)();return i(),a()}}},Ct={map:function(t){return function(r){return function(e,n){return r(e,function(a){return n(t(a))})}}}},MP=A(Ct),Fp=function(t){return function(r){return function(e,n){return r(e,function(a){var u=t(a);if(u instanceof h)return n(u.value0);if(u instanceof F)return void 0;throw new Error("Failed pattern match at FRP.Event (line 201, column 31 - line 203, column 35): "+[u.constructor.name])})}}},ld=function(t){return Fp(function(r){var e=t(r);if(e)return new h(r);if(!e)return F.value;throw new Error("Failed pattern match at FRP.Event (line 129, column 13 - line 131, column 25): "+[e.constructor.name])})},Hh=function(){var r=rd(),e=Nr(0)();return{event:function(n,a){var u=Nr(a)(),i=zr(e)();return ed(i,u,r),ni(function(o){return o+1|0})(e)(),function(){return te(Uh)(u)(),nd(i,r),void 0}},push:function(n){return td(r,function(a){var u=zr(a)();return u(n)})}}},IP=function(t){return function(r,e){var n=Hh(),a=t(n.event),u=n.event(r,e),i=a(r,n.push);return function(){return i(),u()}}},zh={compact:Fp(ot(it)),separate:function(t){return{left:Fp(function(r){if(r instanceof Ht)return new h(r.value0);if(r instanceof zt)return F.value;throw new Error("Failed pattern match at FRP.Event (line 112, column 13 - line 114, column 33): "+[r.constructor.name])})(t),right:Fp(function(r){if(r instanceof zt)return new h(r.value0);if(r instanceof Ht)return F.value;throw new Error("Failed pattern match at FRP.Event (line 119, column 13 - line 121, column 32): "+[r.constructor.name])})(t)}}},cu={filter:ld,filterMap:Fp,partition:function(t){return function(r){return{yes:ld(t)(r),no:ld(function(e){return!t(e)})(r)}}},partitionMap:function(t){return function(r){return{left:pi(cu)(function(){var e=la(h.create)(g(F.value));return function(n){return e(t(n))}}())(r),right:pi(cu)(function(e){return ss(t(e))})(r)}}},Compactable0:function(){return zh},Functor1:function(){return Ct}},PP=function(t){return function(r){return function(e,n){var a=Nr(F.value)(),u=Ki(Vf)(),i=Nr(F.value)(),o=Ki(Vf)(),f=Nr(!0)(),p=t(e,function(s){var c=zr(f)();if(c)return Ep(Ki(Wi(s)(u)))();te(new h(s))(a)();var v=zr(i)();return Hs(v)(function(K){return function(){return n(K(s))}})()}),l=r(e,function(s){var c=zr(f)();if(c)return Ep(Ki(Wi(s)(o)))();te(new h(s))(i)();var v=zr(a)();return Hs(v)(function(K){return function(){return n(s(K))}})()});te(!1)(f)();var _=Ki(bs(u))(),m=Ki(bs(o))();return function(){return _.length===0?te(UD(m))(i)():Rs(_,function(s){return te(new h(s))(a)(),Rs(m,function(c){return te(new h(c))(i)(),n(c(s))})})}(),Ki(ds(0)(Rn(_))([])(u))(),Ki(ds(0)(Rn(m))([])(o))(),function(){return p(),l()}}}},Pe=function(t){return function(r){return r}(_d(321).subscribe)(t)},_d=Wh("backdoor","FRP.Event",function(){var t=function(){var e=rd(),n=Nr(0)();return{event:function(a,u){var i=Nr(u)(),o=zr(n)();return ed(o,i,e),ni(function(f){return f+1|0})(n)(),function(){return te(Uh)(i)(),nd(o,e),void 0}},push:function(a){return function(){return td(e,function(u){var i=zr(u)();return i(a)})}}}};return{makeEvent:function(){var r=function(e){return function(n,a){return n?Zf(void 0):e(function(u){return function(){return a(u)}})()}};return r}(),makeEventO:function(){var r=function(e){return function(n,a){return n?Zf(void 0):e(a)}};return r}(),makePureEvent:function(){var r=function(e){return function(n,a){return e(function(u){return function(){return a(u)}})()}};return r}(),makeLemmingEvent:function(){var r=function(e){return function(n,a){var u=function(i){return function(o){return function(){return i(n,Qi(o))}}};return e(u)(function(i){return function(){return a(i)}})()}};return r}(),makeLemmingEventO:function(){var r=function(e){return function(n,a){var u=function(i,o){return i(n,o)};return e(u,a)}};return r}(),create:t,createPure:t,subscribe:function(){var r=function(e){return function(n){return function(){return e(!1,Qi(n))}}};return r}(),subscribeO:function(){var r=function(e,n){return e(!1,n)};return r}(),subscribePureO:function(){var r=function(e,n){return e(!0,n)};return r}(),subscribePure:function(){var r=function(){var e=function(n){return function(a){return function(){return n(!0,Qi(a))}}};return e}();return r}(),bus:function(){var r=function(e){return function(n,a){var u=pd(722)();return a(e(u.push)(u.event)),Zf(void 0)}};return r}(),memoize:function(){var r=function(e){return function(n){return function(a,u){var i=Hh();return u(n(i.event)),e(a,i.push)}}};return r}(),hot:function(){var r=function(e){return function(){var a=pd(740)(),u=Pe(e)(a.push)();return{event:a.event,unsubscribe:u}}};return r}(),mailboxed:function(){var r=function(e){var n=Kf(e),a=Ps(e);return function(u){return function(i){return function(o,f){var p=Nr(ji)();f(i(function(_){return function(m,s){return Ep(zf(n(function(c){if(c instanceof F)return new h([s]);if(c instanceof h)return new h(hP(c.value0)([s]));throw new Error("Failed pattern match at FRP.Event (line 753, column 21 - line 755, column 55): "+[c.constructor.name])})(_))(p))(),Ep(zf(n(function(c){if(c instanceof F)return F.value;if(c instanceof h)return new h(zD(cd)(s)(c.value0));throw new Error("Failed pattern match at FRP.Event (line 762, column 21 - line 764, column 69): "+[c.constructor.name])})(_))(p))}}));var l=u(o,function(_){var m=zr(p)(),s=a(_.address)(m);if(s instanceof F)return void 0;if(s instanceof h)return Rs(s.value0,function(c){return c(_.payload)});throw new Error("Failed pattern match at FRP.Event (line 771, column 13 - line 773, column 99): "+[s.constructor.name])});return function(){return Ep(te(ji)(p))(),l()}}}}};return r}(),delay:function(){var r=function(e){return function(n){return function(a,u){var i=Nr(CP)(),o=n(a,function(f){var p=Nr(F.value)(),l=Bs(e)(function(){u(f);var m=zr(p)();return Ft(Zf(void 0))(function(s){return ni(xP(s))(i)})(m)()})();return te(new h(l))(p)(),ni(TP(Eh(l)))(i)()});return function(){var p=zr(i)();return SP(p)(Sp)(),o()}}}};return r}()}}),pd=Wh("create","FRP.Event",function(){return function(){return void 0,function(r){return r}(_d(437).create)()}}),Hc=_d(583),Vh=pd(434),sd=function(t){return function(r){return r}(Hc.bus)(t)};var lu=function(t){return function(r){return r}(Hc.delay)(t)};var Yn=function(t){return function(r){return r}(Hc.makeEvent)(t)};var Jh=function(t){return function(r){return r}(Hc.makeLemmingEvent)(t)},hr=function(t){return function(r){return r}(Hc.makeLemmingEventO)(t)};var Gu=function(t){return function(r){return r}(Hc.memoize)(t)};var yn={apply:function(t){return function(r){return PP(t)(MP(Rf)(r))}},Functor0:function(){return Ct}};var nt={pure:function(t){return function(r,e){return e(t),Zf(void 0)}},Apply0:function(){return yn}};var jt={alt:function(t){return function(r){return function(e,n){return EP(FP(function(a){return function(u){return function(){return a(),u()}}})(function(){return t(e,n)}))(function(){return r(e,n)})()}}},Functor0:function(){return Ct}};var T={empty:function(t,r){return Zf(void 0)},Alt0:function(){return jt}},NP={Applicative0:function(){return nt},Plus1:function(){return T}},$e={keepLatest:wP,sampleOnRight:$P,sampleOnLeft:OP,fix:IP,Alternative0:function(){return NP},Filterable1:function(){return cu}};var RP=function(t){return t},$p=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Op=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),LP=function(t){return t},zs=M(),d=LP;var P=function(){return $p.create}();var rt=function(){return Op.create}(),Kr=function(){var t=A(ya)(A(R)(g(!0)));return function(r){return RP(t(r))}}(),kt=function(t){return t.attr};function Gh(t){return()=>t.slice()}function jh(t){return r=>e=>()=>{e[t]=r}}function Xh(t){return()=>t.slice()}function wp(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var Kh=function(t){var r=se(t);return function(){return function(){return function(e){return function(n){return function(a){return wu(r(e))(n)(a)}}}}}};var Yh=function(){return function(){return function(t){return function(r){return wp(t,r)}}}},Mp=function(t){var r=se(t);return function(){return function(){return function(e){return function(n){return function(a){return wu(r(e))(n)(a)}}}}}},To=function(t){var r=se(t);return function(){return function(e){return function(n){return Aa(r(e))(n)}}}};var Zh=C(je);var An={vb:function(t){return Zh(new G({},{}))}},Vs=function(t){return t.vb},zP={vbus:function(){var t=function(){return function(n){var a=Vs(n);return function(u){return function(i){return Jh(function(o){return function(f){return function(){var l=a(D.value)();return f(i(l.value0)(l.value1))(),Zh(void 0)}}})}}}},r=t(),e=function(){return function(n){return r(n)}};return e}()},md=function(){return function(t){return function(r){return function(e){return e()(t)}(zP.vbus)(r)}}},pu=function(t){var r=Mp(t)()();return function(){return function(){return function(){return function(e){var n=Vs(e);return function(a){var u=Vs(a);return function(){return function(){return{vb:function(i){return function(){var f=u(D.value)(),p=n(D.value)();return new G(r(D.value)(p.value0)(f.value0),r(D.value)(p.value1)(f.value1))}}}}}}}}}}},re=function(t){var r=Mp(t)()();return function(){return function(){return function(e){var n=Vs(e);return function(){return function(){return{vb:function(a){return function(){var i=n(D.value)(),o=Vh();return new G(r(D.value)(o.push)(i.value0),r(D.value)(o.event)(i.value1))}}}}}}}}};var So=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Ip=function(){function t(){}return t.value=new t,t}();var zc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),tc=function(){function t(){}return t.value=new t,t}(),Dd=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),dd=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Pp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Fo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),S=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var t0=function(t){return t};var rc={eq:function(t){return function(r){return t instanceof So&&r instanceof So?t.value0===r.value0:t instanceof Ip&&r instanceof Ip}}};var I=function(t){return new Pp(t)},Np=function(t){return new Fo(t)},Rp=function(t){return new dd(t)};var r0=t=>r=>r[t];var ec=function(t){return t.reflectType};var n0=function(t){return ec(t)};var Lp=Ar;var Gs=function(){return function(t){return t}};var bd=function(t){return[t]};var yd=function(){return function(){return function(){return function(){return function(){return function(t){var r=n0(t);return function(e){return r0(r(e))}}}}}}};var Ad=[];var i0=function(){return function(){return function(t){return function(r){return Hi(t)(r)}}}};function f0(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var $o={};function kd(t){return t()}function c0(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function l0(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function p0(t){return function(r){return function(e){return function(n){var a=e;function u(o){return function(f){return r(f)(o)(n[o])}}for(var i in n)hasOwnProperty.call(n,i)&&(a=t(a)(u(i)));return a}}}}function Bp(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var XP=Object.keys||Bp(function(t){return function(){return t}});function gd(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var hd=function(t){return function(r){return function(){return delete r[t],r}}};var s0=ue(Gt),eN=ot(it);var Cd=Bp(function(t){return function(r){return r}});var nN=f0;var v0=function(t){return function(r){return kd(function(){var n=nN(r)();return t(n)(),n})}};var m0=function(t){return function(r){return l0(r,t)}};var vi=function(t){return function(r){return v0(gd(t)(r))}},Xs={map:function(t){return function(r){return c0(r,t)}}},aN={mapWithIndex:m0,Functor0:function(){return Xs}},xd=function(){return et};var Qs=p0(Rf),D0=function(t){var r=vt(t.Semigroup0()),e=pr(t);return function(n){return Qs(function(a){return function(u){return function(i){return r(a)(n(u)(i))}}})(e)}},Wp={foldl:function(t){return Qs(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return s0(t)(r)(Cd(e))}}},foldMap:function(t){var r=D0(t);return function(e){return r(g(e))}}},d0={foldlWithIndex:function(t){return Qs(lr(t))},foldrWithIndex:function(t){return function(r){return function(e){return s0(wc(t))(r)(Bp(G.create)(e))}}},foldMapWithIndex:function(t){return D0(t)},Foldable0:function(){return Wp}},uN={traverseWithIndex:function(t){var r=t.Apply0(),e=Dt(r),n=A(r.Functor0()),a=C(t);return function(u){return function(i){return Qs(function(o){return function(f){return function(p){return e(n(lr(vi(f)))(o))(u(f)(p))}}})(a($o))(i)}}},FunctorWithIndex0:function(){return aN},FoldableWithIndex1:function(){return d0},Traversable2:function(){return Vc}},Vc={traverse:function(t){var r=zi(uN)(t);return function(e){return r(g(e))}},sequence:function(t){return bn(Vc)(t)(eN)},Functor0:function(){return Xs},Foldable1:function(){return Wp}};var Td=function(t){return v0(hd(t))};function b0(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function y0(t){return function(r){return function(e){return e[t]=r,e}}}function A0(t){return function(r){return function(e){return e[t]=r(e[t]),e}}}function k0(t){return function(r){return delete r[t],r}}var qp=Zo;var g0=function(){return function(){return function(t){var r=se(t);return function(e){return function(n){return function(a){return A0(r(e))(n)(a)}}}}}};var Up=function(){return function(){return function(t){var r=se(t);return function(e){return function(n){return function(a){return y0(r(e))(n)(a)}}}}}};var h0=function(t){var r=se(t);return function(){return function(){return function(e){return function(n){return k0(r(e))(n)}}}}},Sd=it,Ks=function(t){return function(r){return t(b0(r))}},C0=lr(Ks)({});var iN=Co($e),fN=A(Ct),cN=me(Gt)(T);var lN=yt(Ec),pN=A(Ar),Fd=Gs(),_N=bo(ii),sN=cD(Jk),nc=C(je),S0=A(pa),vN=A(Lp),E0=Ua(je),Od=E0(Ue),x0=Vl(qp),mN=Up()()({reflectSymbol:function(){return"id"}}),DN=g0()()({reflectSymbol:function(){return"parent"}}),dN=h0({reflectSymbol:function(){return"raiseId"}})()(),su=Wr(pa),F0=Nn(je),$0=E0(Gt),bN=ot(it),yN=Hn(Gt)(T),AN=dn(je)(Gt),kN=vt(rn),gN=oe(Wp),hN=or(gD),T0=function(){function t(){}return t.value=new t,t}(),$d=function(){function t(){}return t.value=new t,t}(),CN=function(){function t(){}return t.value=new t,t}();var xa=function(t){return function(r){return function(e){var n=function(a){return a(r)(e)};return function(a){if(a instanceof Pp)return yN(xa(t)(r)(e))(a.value0);if(a instanceof Fo)return iN(fN(xa(t)(r)(e))(a.value0));if(a instanceof S)return n(t.toElt(a.value0));if(a instanceof dd)return hr(function(u,i){var o=We($o)(),f=u(a.value0,function(p){var l=t.ids(e)(),_=We(nc(void 0))(),m=t.ids(e)(),s=We(nc(void 0))(),c=We([])(),v=We(nc(void 0))(),K=S0(So.create)(t.ids(e))(),pt=We(T0.value)(),xt=u(p,function(ce){var Z=pn(pt)();if(ce instanceof Dd&&Z instanceof $d){var Xt=pn(c)();return AN(function(Jr){return function(){return i(t.doLogic(ce.value0)(e)(Jr))}})(Xt)()}if(ce instanceof tc&&Z instanceof $d){su(Wa(CN.value)(pt))();var At=function(){var fn=pn(c)();$0(fn)(function(xe){return Od(r.parent)(function(pe){return function(){return i(t.disconnectElement(e)({id:xe,parent:pe,scope:K}))}})})();var In=pn(_)();In();var nu=pn(s)();return nu(),su(qu(Td(l))(o))(),su(qu(Td(m))(o))()};return su(Wa(At)(v))(),At()}if(ce instanceof zc&&Z instanceof T0){su(Wa($d.value)(pt))();var jr=u(xa(t)(function(){var Jr={};for(var fn in r)({}).hasOwnProperty.call(r,fn)&&(Jr[fn]=r[fn]);return Jr.scope=K,Jr.raiseId=function(In){return su(qu(kN([In]))(c))},Jr}())(e)(ce.value0),i);return su(qu(vi(m)(jr))(o))(),su(Wa(jr)(s))()}return void 0});su(Wa(xt)(_))(),su(qu(vi(l)(xt))(o))();var xr=pn(v)();return xr()});return function(){return lN(pn(o))(gN(hN)(nc(void 0)))(),f()}});throw new Error("Failed pattern match at Bolson.Control (line 544, column 17 - line 630, column 20): "+[a.constructor.name])}}}},xN=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){var i=function(o){return function(f){return hr(function(p,l){var _=Gh(pN(g(""))(Fd(a)))(),m=cN(_N(function(xr){return sN(function(ce){return function(Z){return Z instanceof S?function(Xt){return Xt(function(){var At={};for(var jr in o)({}).hasOwnProperty.call(o,jr)&&(At[jr]=o[jr]);return At.parent=F.value,At.scope=r(o.scope),At.raiseId=function(Jr){return jh(xr)(Jr)(_)},At}())(f)}(n.toElt(Z.value0)):ce(n.wrapElt(Z))}})})(Fd(a))),s=p(m,l),c=We(nc(void 0))(),v=S0(et)(Xh(_))(),K=vN(function(xr){return function(ce){return new S(n.fromEltO1(function(Z){return function(Xt){return hr(function(At,jr){return Z.raiseId(xr)(),Od(Z.parent)(function(Jr){return function(){return jr(n.giveNewParent(Xt)(Ks(x0(mN(D.value)(xr))(x0(DN(D.value)(g(Jr)))(dN(D.value))))(Z))(ce))}})(),nc(void 0)})}}))}})(v),pt=xa(e)(o)(f)(u(K)(et)),xt=p(pt,l);return su(Wa(xt)(c))(),function(){s(),F0(!t)($0(Fd(v))(function(Z){return function(){return l(n.deleteFromCache(f)({id:Z}))}}))();var ce=pn(c)();return ce()}})}};return new S(n.fromEltO2(i))}}}}}}}}},TN=xN()()();var wd=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return TN(!1)(bN)(t)(r)(e)(n)}}}}}}};var O0=function(t){return function(r){return function(e){var n=function(a){return function(u){return hr(function(i,o){var f=We(F.value)(),p=e(new S(r.fromElt(function(l){return function(_){return hr(function(m,s){return function(){var v=pn(f)();if(v instanceof F)return void 0;if(v instanceof h)return Od(l.parent)(function(K){return F0(v.value0!==K)(function(){return l.raiseId(v.value0)(),s(r.connectToParent(u)({id:v.value0,parent:K}))})})();throw new Error("Failed pattern match at Bolson.Control (line 655, column 27 - line 663, column 16): "+[v.constructor.name])}(),nc(void 0)})}})));return i(xa(t)(function(){var l={};for(var _ in a)({}).hasOwnProperty.call(a,_)&&(l[_]=a[_]);return l.parent=a.parent,l.scope=a.scope,l.raiseId=function(m){return function(){return a.raiseId(m)(),su(Wa(new h(m))(f))()}},l}())(u)(p),o)})}};return new S(r.fromElt(n))}}};var SN=ot(it);var Fn={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},Oo=function(t){return t.dimap},Ta=function(t){var r=Oo(t);return function(e){return r(e)(SN)}};var Id=M(),EN=md(),FN=Ta(Fn),$N=A(Ct),w0=he(),ON=Mt(rc),wN=C(je),Md=C(nt),MN=w(T),IN=me(Gt)(T);var vu=function(){return function(t){var r=EN(t);return function(e){return function(n){return new Fo(Id(r(e)(n)))}}}};var M0=function(t){return function(r){var e=function(n){return n instanceof S?new S(FN(function(a){return{pos:t,dynFamily:a.dynFamily,ez:a.ez,parent:a.parent,raiseId:a.raiseId,scope:a.scope}})(n.value0)):n instanceof Fo?new Fo($N(e)(n.value0)):r};return e(r)}},z=function(t){return M0(h.create(t))};var PN=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(t){return function(r){return r.ids}(w0(t))},disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:ON})}},toElt:function(t){return t}}},NN=PN(),I0=function(t){return new zc(w0(M0(F.value)(t)))};var Pd=function(t){return sd(t)};var He=function(t){return new Fo(Id(Pd(t)))},P0=function(t){return He(Jf(t))},RN=function(t){return function(r){return function(e){return xa(NN)(t)(r)(e)}}},LN=function(t){return function(r){var e=function(n){return function(a){return hr(function(u,i){var o=a.ids();n.raiseId(o)();var f=function(){if(n.parent instanceof F){var l=a.ids();return new G(Md(a.makeElement({id:l,parent:F.value,scope:n.scope,tag:"div",pos:F.value,dynFamily:F.value})),l)}if(n.parent instanceof h)return new G(MN,n.parent.value0);throw new Error("Failed pattern match at Deku.Core (line 339, column 34 - line 353, column 36): "+[n.parent.constructor.name])}(),p=u(IN([f.value0,Md(a.makeDynBeacon({id:o,parent:new h(f.value1),scope:n.scope,dynFamily:n.dynFamily,pos:n.pos})),Md(a.attributeParent({id:o,parent:f.value1,pos:n.pos,dynFamily:n.dynFamily,ez:n.ez})),RN({parent:new h(f.value1),scope:n.scope,ez:!1,raiseId:function(l){return wN(void 0)},pos:F.value,dynFamily:new h(o)})(a)(t(r))]),i);return function(){return i(a.removeDynBeacon({id:o})),p()}})}};return new S(e)}},N0=LN(Id(Rp));var Ys=A(Ct),Nd=me(Gt)(T),ac=C(nt),qN=w(T),R0=C(je),UN=Uc($e),HN=Co($e),zN=xp(cu),VN=Mt(ti),JN=M(),GN=he(),jN=Mt(rc);var L0=$t(jt);var XN=vt(rn);var QN=function(t){return function(r){return function(e){return Ys(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},KN=function(t){return function(r){return function(e){return Ys(function(n){return function(a){if(a.value instanceof $p)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof Op)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 62, column 26 - line 64, column 45): "+[a.value.constructor.name])}(zs(n))})(e)}}},Qe=function(t){var r=function(e){return function(n){return hr(function(a,u){var i=n.ids();e.raiseId(i)();var o=a(Nd([ac(n.makeText({id:i,parent:e.parent,pos:e.pos,scope:e.scope,dynFamily:e.dynFamily})),QN(n)(i)(t),Ft(qN)(function(f){return ac(n.attributeParent({id:i,parent:f,pos:e.pos,dynFamily:e.dynFamily,ez:e.ez}))})(e.parent)]),u);return function(){return u(n.deleteFromCache({id:i})),o()}})}};return new S(r)},ee=function(t){return Qe(ac(t))},B0=function(t){return function(r){var e=function(){var n=function(a){return function(u){return new G(a+1|0,new G(u,a))}};return UN(n)(0)}();return N0(HN(Gu(e(r))(function(n){return Ys(function(a){return Nd([Ys(g(tc.value))(zN(function(){var u=VN(a.value1+1|0);return function(i){return u(qe(i))}}())(n)),ac(I0(JN(t(a.value0))))])})(n)})))}},YN=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(t){return function(r){return r.ids}(GN(t))},disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:jN})}},toElt:function(t){return t}}},ZN=YN();var W0=function(t){return function(r){return function(e){return xa(ZN)(t)(r)(e)}}},q0=function(t){return function(r){return function(e){return hr(function(n,a){return n(L0(ac(e.makeRoot({id:"deku-root",root:t})))(W0({parent:new h("deku-root"),scope:new So("rootScope"),raiseId:function(u){return R0(void 0)},ez:!0,pos:F.value,dynFamily:F.value})(e)(r)),a)})}}},V=function(t){return function(r){return function(e){var n=function(a){return function(u){return hr(function(i,o){var f=u.ids();a.raiseId(f)();var p=i(L0(Nd(XN([ac(u.makeElement({id:f,parent:a.parent,scope:a.scope,tag:t,pos:a.pos,dynFamily:a.dynFamily})),KN(u)(f)(r)])(Ft([])(function(l){return[ac(u.attributeParent({id:f,parent:l,pos:a.pos,dynFamily:a.dynFamily,ez:a.ez}))]})(a.parent))))(W0({parent:new h(f),scope:a.scope,ez:!0,raiseId:function(l){return R0(void 0)},pos:F.value,dynFamily:F.value})(u)(e)),o);return function(){return o(u.deleteFromCache({id:f})),p()}})}};return n}}};var ie=function(){function t(){}return t.value=new t,t}();var Se={attr:function(t){return function(r){return d({key:"click",value:rt(r)})}}};var It=function(){function t(){}return t.value=new t,t}();var Zs={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}};var U0={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}};var Cr={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}};var H0={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}},Jc={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}};var z0={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}};var V0={attr:function(t){return function(r){return d({key:"style",value:P(r)})}}};var J0=M(),Rd=function(t){return function(r){return new S(V("a")(t)(J0(I(J0(U(z)(r))))))}};var G0=M(),yr=function(t){return function(r){return new S(V("div")(t)(G0(I(G0(U(z)(r))))))}},Ur=yr(w(T));var j0=M(),jc=function(t){return function(r){return new S(V("span")(t)(j0(I(j0(U(z)(r))))))}},Ld=jc(w(T));var rR=M(),eR=$t(jt),nR=C(nt);var aR=function(t){return function(r){return P0(function(e){return Np(rR(Gu(t(e.value1))(function(n){return r(new G(e.value0,n))})))})}},Q0=function(t){return aR(function(r){return eR(nR(t))(r)})};var K0=function(t){return function(r){return t(r)}};var Y0=t=>r=>e=>()=>{if(e.units[r.id]){let n=e.units[r.parent].main;e.units[r.id].main&&e.units[r.id].main.parentNode||e.units[r.id].startBeacon&&e.units[r.id].startBeacon.parentNode||(r.ez?(()=>(e.units[r.id].main?n.appendChild(e.units[r.id].main):(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)),!0))():t(r.pos)(u=>()=>t(r.dynFamily)(i=>()=>{for(var o=0,f=0,p;f<n.childNodes.length;){if(n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+i){f+=1;break}f++}let l=m=>{e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,n.childNodes[m]),n.insertBefore(e.units[r.id].endBeacon,n.childNodes[m])):n.insertBefore(e.units[r.id].main,n.childNodes[m])};for(;f<n.childNodes.length;){var _;if((_=n.childNodes[f].$dekuId)&&t(e.units[_].dynFamily)(s=>()=>t(e.units[_].pos)(v=>()=>i===s&&u<=v?(l(f),!0):!1)())())return!0;if(o===u||n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+i+"%-%")return l(f),!0;n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue.substring(0,3)==="%-%"&&!p&&(p=n.childNodes[f].nodeValue+"%-%"),p||o++,n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue===p&&(p=void 0,o++),f++}return!1})())())||(r.parent.indexOf("@!%")!==-1?t(r.dynFamily)(i=>()=>(e.units[r.id].main?e.units[i].endBeacon.parentNode.insertBefore(e.units[r.id].main,e.units[i].endBeacon):(e.units[i].endBeacon.parentNode.insertBefore(e.units[r.id].endBeacon,e.units[i].endBeacon),e.units[i].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon)),!0))()||(e.units[r.id].main?n.parentNode.replaceChild(e.units[r.id].main,n):(n.parentNode.replaceChild(e.units[r.id].endBeacon,n),e.units[r.id].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon))):t(r.dynFamily)(i=>()=>(e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,e.units[i].endBeacon),n.insertBefore(e.units[r.id].endBeacon,e.units[i].endBeacon)):n.insertBefore(e.units[r.id].main,e.units[i].endBeacon),!0))()||(e.units[r.id].startBeacon?(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)):n.appendChild(e.units[r.id].main)))}};var Z0=t=>r=>e=>n=>()=>{var a,u,i=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(i),!t(e.parent)(()=>()=>n.hydrating&&r&&(a=n.allBeacons[e.id])&&(u=n.allBeacons[`${e.id}%-%`])?(n.units[i]={listeners:{},parent:e.parent,scope:e.scope,pos:e.pos,dynFamily:e.dynFamily,startBeacon:a,endBeacon:u},a.$dekuId=i,u.$dekuId=i,!0):!1)()){let f=document.createComment(`%-%${e.id}`),p=document.createComment(`%-%${e.id}%-%`);n.units[i]={listeners:{},parent:e.parent,dynFamily:e.dynFamily,scope:e.scope,pos:e.pos,startBeacon:f,endBeacon:p},f.$dekuId=i,p.$dekuId=i}};var tC=t=>r=>()=>r.units[t]&&r.units[t].dynFamily?r.units[t].dynFamily:(()=>{throw new Error(`No positional information for ${t}`)})(),rC=t=>r=>()=>r.units[t]&&r.units[t].main&&r.units[t].main.parentNode&&r.units[t].main.parentNode.$dekuId?r.units[t].main.parentNode.$dekuId:r.units[t]&&r.units[t].startBeacon&&r.units[t].startBeacon.parentNode&&r.units[t].startBeacon.parentNode.$dekuId?r.units[t].startBeacon.parentNode.$dekuId:(()=>{throw new Error(`No parent information for ${t}`)})(),eC=t=>r=>()=>r.units[t]&&r.units[t].scope?r.units[t].scope:(()=>{throw new Error(`No scope information for ${t}`)})(),nC=t=>r=>e=>n=>()=>{var a,u=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(u),!t(e.parent)(()=>()=>n.hydrating&&r&&(a=document.documentElement.querySelector(`[data-deku-ssr="${u}"]`))?(n.units[u]={listeners:{},pos:e.pos,parent:e.parent,scope:e.scope,dynFamily:e.dynFamily,main:a},a.$dekuId=u,!0):!1)()){let o=document.createElement(e.tag);n.units[u]={listeners:{},parent:e.parent,pos:e.pos,scope:e.scope,dynFamily:e.dynFamily,main:o},o.$dekuId=u}},aC=t=>r=>e=>n=>a=>()=>{var u=n.id,i;if(a.scopes[n.scope]||(a.scopes[n.scope]=[]),a.scopes[n.scope].push(u),!t(n.parent)(f=>()=>{if(a.hydrating&&r&&(i=document.documentElement.querySelector(`[data-deku-ssr="${f}"]`))){for(var p=0;p<i.childNodes.length;p++){let s=u.split("@-@");if(i.childNodes[p].nodeType===8&&i.childNodes[p].nodeValue===s[0]){p=p-1;var l=p===-1,_=p>=0&&i.childNodes[p].nodeType===8;l&&i.prepend(document.createTextNode("")),_&&i.insertBefore(document.createTextNode(""),i.childNodes[p+1]);break}}let m=i.childNodes[p];return a.units[u]={main:m,pos:n.pos,parent:n.parent,scope:n.scope},m.$dekuId=u,!0}return!1})()){let f=document.createTextNode("");a.units[u]={main:f,parent:n.parent,scope:n.scope,pos:n.pos,dynFamily:n.dynFamily},f.$dekuId=u}};function Bd(){return{units:{},scopes:{},allBeacons:{}}}var uC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,a=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":r.key==="disabled"?e.units[n].main.disabled=a==="true":e.units[n].main.setAttribute(r.key,a)}},oC=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,a=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=i=>a(i)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}}},iC=t=>r=>()=>{if(r.units[t.id]){var e=t.id;r.units[e].main.nodeValue=t.text}},fC=t=>r=>e=>n=>a=>()=>{var u,i,o=n.id,f=n.html,p=n.verb,l=n.cache,_=n.parent,m=n.scope,s=n.pxScope;let c=t(n.parent)(()=>()=>a.hydrating&&r&&(u=document.documentElement.querySelector(`[data-deku-ssr="${o}"]`))?(a.units[o]={listeners:{},pos:n.pos,scope:m,parent:_,main:u},u.$dekuId=o,!0):!1)();if(!c){let K=Object.entries(l);for(var v=0;v<K.length;v++){let pt=K[v][0];K[v][1]===!0?f=f.replace(p+pt+p,'data-deku-attr-internal="'+pt+'"'):f=f.replace(p+pt+p,'<span style="display:contents;" data-deku-elt-internal="'+pt+'"></span>')}i=document.createElement("div"),i.innerHTML=f.trim(),a.units[o]={listeners:{},pos:n.pos,scope:m,parent:_,main:i.firstChild},i.firstChild.$dekuId=o}a.scopes[m]||(a.scopes[m]=[]),a.scopes[m].push(o),i||(i=u),i.querySelectorAll("[data-deku-attr-internal]").forEach(function(K){var pt=K.getAttribute("data-deku-attr-internal");let xt=pt+"@!%"+s;a.units[xt]={listeners:{},main:K,scope:m},a.scopes[m].push(xt)}),i.querySelectorAll("[data-deku-elt-internal]").forEach(function(K){var pt=K.getAttribute("data-deku-elt-internal");let xt=pt+"@!%"+s;a.units[pt+"@!%"+s]={listeners:{},main:K,scope:m},a.scopes[m].push(xt)}),c||a.units[o].main.remove()},cC=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root},t.root.$dekuId=e},Wd=t=>r=>e=>n=>()=>{let a=(c,v,K)=>{if(n.units[c].startBeacon){var pt=n.units[c].startBeacon,xt=pt.nextSibling;for(n.units[v].main.insertBefore(pt,K),pt=xt;pt&&pt!==n.units[c].endBeacon;)xt=pt.nextSibling,n.units[v].main.insertBefore(pt,K),pt=xt}else n.units[v].main.insertBefore(n.units[c].main,K)},u=[];u.push(e);for(var i=0;i<u.length;i++){let c=u[i],v=c.id,K=c.parent;n.units[v].containingScope=c.scope;var o=void 0;r(c.pos)(xt=>()=>(o=xt,!0))(),o===void 0&&(o=Number.MAX_VALUE);let pt=n.units[K].main.childNodes;for(var f=0,p=!1,l=0;f<pt.length;){var _;if(_=pt[f].$dekuId){if(r(c.dynFamily)(xr=>()=>p?!1:n.units[_].endBeacon===pt[f]&&xr===_?(n.units[v].pos=t(l),a(v,K,pt[f]),!0):!1)()){p=!0;break}if(n.units[_].dynFamily!==n.units[v].dynFamily){f++;continue}if(p){f++;continue}l===o?(a(v,K,pt[f]),l++,p=!0):n.units[_].endBeacon!==pt[f]&&(n.units[_].pos=t(l),l++)}f++}if(p)return;if(n.units[v].main)n.units[K].main.appendChild(n.units[v].main);else{var m=n.units[v].startBeacon,s=m.nextSibling;for(n.units[K].main.appendChild(m),m=s;m&&m!==n.units[v].endBeacon;)s=m.nextSibling,n.units[K].main.appendChild(m),m=s}}},lC=t=>r=>()=>{if(r.units[t.id]){var e=t.id;if(r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope))return;if(r.units[e].main)r.units[e].main.remove();else{let u=document.createElement("div");var n=r.units[e].startBeacon,a=n.nextSibling;for(u.appendChild(n),n=a;n&&n!==r.units[e].endBeacon;)a=n.nextSibling,u.appendChild(n),n=a}}},qd=t=>r=>()=>{r.units[t.id]&&delete r.units[t.id]},pC=qd;var _C=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},Vr=function(t){return t};var Ud=function(t){return function(r){return Math.pow(t,r)|0}};var tv=isFinite;var zp=Math.floor;var Zi=function(t){return function(r){return Math.pow(t,r)}},Vp=function(t){return function(r){return t%r}},rv=Math.round;var ev=Math.sin;var tf=3.141592653589793;var vC=Bn(Zl),mC=Wn(Zl);var Xc=function(){return _C(h.create)(F.value)}(),DC=function(t){if(!tv(t))return 0;if(t>=Vr(vC))return vC;if(t<=Vr(mC))return mC;if(Yr)return qn(0)(Xc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},dC=function(t){return DC(rv(t))};var Jp=function(t){return DC(zp(t))};var Mo=Math.random;var Gp=function(t){return function(r){return function(){var n=Mo(),a=(Vr(r)-Vr(t)+1)*n+Vr(t);return Jp(a)}}};var sR=ha(Ru),vR=Sn();var bC=function(t){return t};var mR=1,nv=2147483647,DR=function(){return nv-1|0}(),rf=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,i=sR(a)(u),o=i<e;return o?i+n|0:i}}};return r(mR)(DR)(t)};var dR=0,bR=48271,yC=function(t){return function(r){return vR(Xc(Vp(Vr(bR)*Vr(r)+Vr(t))(Vr(nv))))}},AC=yC(dR);var TR=function(){function t(i){this.fn=i}var r={},e=function(i,o){this.head=i,this.tail=o};function n(i){return new e(i,r)}function a(i){return function(o){return new e(i,o)}}function u(i){for(var o=[],f=i;f!==r;)o.push(f.head),f=f.tail;return o}return function(i){return function(o){return function(f){var p=function(_,m){return i(o(a)(f(_)))(m)},l=function(_,m,s){if(m===0)return _;var c=s[m-1];return new t(function(){var v=l(p(c,_),m-1,s);return v})};return function(_){for(var m=o(n)(f(_[_.length-1])),s=l(m,_.length-1,_);s instanceof t;)s=s.fn();return o(u)(s)}}}}}();var xC=function(t){return t};var TC=rn;var SC=Gt;var BR=Nc();var $C=xC;var Kp=function(t){return t};var Yp=function(t){return $C(rh(t))};var Qc=function(t){if(Rn(t)>0)return new h($C(t));if(Yr)return F.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 160, column 1 - line 160, column 58): "+[t.constructor.name])};var OC=function(t){return function(r){return t(Kp(r))}};var wC=OC(Rn);var MC=function(){return OC(BR)};var mi=function(t){return t.state};function Io(t){return new Error(t)}function t_(t){return function(){throw t}}function ov(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var Xu=function(t){return t.throwError};var fL={throwError:t_,Monad0:function(){return ei}};var qC={catchError:lr(ov),MonadThrow0:function(){return fL}};var Di=function(t){return t.catchError};var iv=function(t){var r=Di(t),e=t.MonadThrow0().Monad0(),n=A(e.Bind1().Apply0().Functor0()),a=C(e.Applicative0());return function(u){return r(n(zt.create)(u))(function(i){return a(Ht.create(i))})}};var fe={liftEffect:ot(it),Monad0:function(){return ei}},Ee=function(t){return t.liftEffect};var sv=function(t){var r=A(t);return{map:function(e){return function(n){return function(a){return r(function(u){return new G(e(u.value0),u.value1)})(n(a))}}}}};var Zd=function(t){return{Applicative0:function(){return Dv(t)},Bind1:function(){return vv(t)}}},vv=function(t){var r=yt(t.Bind1());return{bind:function(e){return function(n){return function(a){return r(e(a))(function(u){var i=n(u.value0);return i(u.value1)})}}},Apply0:function(){return mv(t)}}},mv=function(t){var r=sv(t.Bind1().Apply0().Functor0());return{apply:Na(Zd(t)),Functor0:function(){return r}}},Dv=function(t){var r=C(t.Applicative0());return{pure:function(e){return function(n){return r(new G(e,n))}},Apply0:function(){return mv(t)}}};var tb=function(t){var r=C(t.Applicative0()),e=Zd(t);return{state:function(n){return function(a){return r(n(a))}},Monad0:function(){return e}}};var UC=function(t){return function(r){var e=t(r);return e.value0}};var mL=tb(iu),DL=mi(mL);var dL=vv(iu);var bL=sv(Nu);var yL=ln(jn);var AL=Rr(jn);var kL=MC();var gL=function(t){return t};var VC=function(){var t=function(r){return new G(bC(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=AC(r.newSeed),e}())};return DL(t)}();var o_=bL,u_=A(o_),JC=u_(function(t){return Vr(t)/Vr(nv)})(VC);var uf=function(t){return UC(gL(t))};var dv=dL,hL=yt(dv);var bv=mv(iu),CL=Dt(bv),zC=function(t){return function(r){var e=Vr(r),n=Vr(t),a=function(o){return n+Vp(o)(e-n+1)},u=u_(Vr)(VC),i=CL(u_(AL)(u))(u_(yL(2))(u));return u_(function(o){return Jp(a(o))})(i)}},eb=function(t){return function(r){var e=t<=r;return e?zC(t)(r):zC(r)(t)}};var i_=Dv(iu),xL=C(i_);var nb=function(t){return hL(eb(0)(wC(t)-1|0))(function(r){return xL(kL(t)(r))})};var uc=function(t){return t.arbitrary};var GC={arbitrary:JC};var yv=function(){return{arbitrary:eb(-1e6)(1e6)}}();var ML=Wr(pa);var IL=Rt(ga),PL=uc(yv),NL=Rr(Iu);var RL=C(Yt);var oc=function(t){return function(r){return t instanceof h?r(t.value0):RL(!1)}},LL=function(t){return function(r){return function(){var n=eC(t.id)(r)(),a=rC(t.id)(r)(),u=tC(t.id)(r)(),i={scope:n,parent:a,dynFamily:u,id:t.id,pos:new h(t.pos),ez:!1};return Wd(h.create)(oc)(i)(r)()}}};var jC=function(t){return{ids:function(){var e=pn(t)(),n=IL(uf(PL)({newSeed:rf(e),size:5}));return ML(qu(NL(1))(t))(),n},makeElement:nC(oc)(!1),makeDynBeacon:Z0(oc)(!1),attributeParent:Y0(oc),makeRoot:cC,makeText:aC(oc)(!1)(Ft(void 0)),makePursx:fC(oc)(!1)(Ft(void 0)),setProp:uC(!1),setCb:oC(!1),setText:iC,sendToPos:LL,removeDynBeacon:pC,deleteFromCache:qd,giveNewParent:Wd(h.create)(oc),disconnectElement:lC}};var di=function(){return window};function QC(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var i=Object.getPrototypeOf(u),o=i.constructor.name;if(o===e)return r(n);if(o==="Object")return t;u=i}return t}var Tt=function(t){return function(r){return QC(F.value,h.create,t,r)}};function KC(t,r,e){return t==null?r:e(t)}var ze=function(t){return KC(t,F.value,h.create)};var ub=Tt("HTMLCanvasElement");function tx(t){return t.body}var ZL=A(R);var rx=function(t){return ZL(ze)(function(){return tx(t)})};var ex=et;function ic(t){return function(){return t.valueAsNumber}}var rl=Tt("HTMLInputElement");function ib(t){return function(){return t.document}}function hv(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var fb=et;var ux=yt(Kn),$B=De(R),OB=Ju(Vu),wB=pr(Hf(Hf(Sc))),MB=A(Te),IB=Wr(R);var PB=function(t){return function(r){return function(){var n=Bd(),a=$B(OB(We(0)))(function(){var u=q0(t)(r);return function(i){return u(jC(i))}}())();return Pe(a)(function(u){return u(n)})()}}},NB=function(t){return function(){var e=ux(ux(di)(ib))(rx)();return Ft(wB)(function(n){return PB(n)(t)})(MB(ex)(e))()}},ox=function(t){return IB(NB(t))};var fx=w(T),cx=$t(jt),LB=A(Ct),BB=Ta(Fn),WB=me(Gt)(T),ix=C(nt),qB=C(je),UB={reflectType:function(){return"~"}},HB=he(),zB=Mt(rc),VB=M(),JB=function(t){return t};var J={pursxToElement:function(t){return function(r){return function(e){return{cache:$o,element:function(n){return function(a){return fx}}}}}}},cb=function(t){return t.pursxToElement},Ke=function(){return function(t){var r=cb(t);return function(e){var n=ec(e);return function(a){var u=To(a)();return{pursxToElement:function(i){return function(o){return function(f){var p=r(i)(D.value)(f);return{cache:vi(n(D.value))(!0)(p.cache),element:function(l){return function(_){return cx(LB(BB(zs)(function(m){if(m.value instanceof $p)return _.setProp({id:n(D.value)+("@!%"+i),key:m.key,value:m.value.value0});if(m.value instanceof Op)return _.setCb({id:n(D.value)+("@!%"+i),key:m.key,value:m.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4196, column 38 - line 4206, column 24): "+[m.value.constructor.name])}))(u(D.value)(f)))(p.element(l)(_))}}}}}}}}}}};var $=JB,nr=function(t){var r=ec(t);return function(e){var n=ec(e);return function(){return function(){return function(a){var u=cb(a);return function(i){return function(o){return function(f){var p=function(l){return function(_){return hr(function(m,s){var c=_.ids(),v=_.ids();l.raiseId(c)();var K=u(v)(D.value)(f),pt=m(WB([ix(_.makePursx({id:c,parent:l.parent,cache:K.cache,dynFamily:l.dynFamily,pos:l.pos,pxScope:v,scope:l.scope,html:r(o),verb:n(i)})),K.element(l)(_),Ft(fx)(function(xt){return ix(_.attributeParent({id:c,parent:xt,pos:l.pos,dynFamily:l.dynFamily,ez:!1}))})(l.parent)]),s);return function(){return s(_.deleteFromCache({id:c})),pt()}})}};return new S(p)}}}}}}}},gt=function(t){var r=nr(t)(UB)()();return function(){return function(){return function(e){return r(e)(D.value)}}}};var GB=function(t){return function(r){return function(e){return xa({doLogic:function(n){return function(a){return function(u){return a.sendToPos({id:u,pos:n})}}},ids:function(n){return function(a){return a.ids}(HB(n))},disconnectElement:function(n){return function(a){return n.disconnectElement({id:a.id,scope:a.scope,parent:a.parent,scopeEq:zB})}},toElt:function(n){return n}})(t)(r)(VB(e))}}},Q=function(){return function(t){var r=cb(t);return function(e){var n=ec(e);return function(a){var u=To(a)();return{pursxToElement:function(i){return function(o){return function(f){var p=u(D.value)(f),l=r(i)(D.value)(f);return{cache:vi(n(D.value))(!1)(l.cache),element:function(_){return function(m){return cx(GB({parent:new h(n(D.value)+("@!%"+i)),scope:_.scope,raiseId:function(s){return qB(void 0)},pos:_.pos,ez:!1,dynFamily:F.value})(m)(p))(l.element(_)(m))}}}}}}}}}}};var jB=Yh()(),XB=ot(Sd),QB=$u(qp),KB=Up()(),$r=function(){return function(){return{defaults:lr(jB)}}},YB=function(t){return t.defaults},Or={convertRecordOptions:function(t){return function(r){return function(e){return XB}}}},lx=function(t){return t.convertRecordOptions},Jn=function(t){return t.convertOptionsWithDefaults},wr=function(){return function(t){var r=lx(t);return{convertOptions:function(e){return function(n){return C0(r(e)(D.value)(n))}}}}},ZB=function(t){return t.convertOptions},ar=function(t){var r=ZB(t);return function(e){var n=YB(e);return{convertOptionsWithDefaults:function(a){return function(u){var i=n(u),o=r(a);return function(f){return i(o(f))}}}}}},t3=function(t){return t.convertOption},St=function(t){var r=lx(t);return function(e){var n=t3(e);return function(){return function(){return function(){return function(a){var u=KB(a),i=To(a)();return{convertRecordOptions:function(o){return function(f){return function(p){return QB(u(D.value)(n(o)(D.value)(i(D.value)(p))))(r(o)(D.value)(p))}}}}}}}}}};var e3=function(){return function(){return function(){return function(t){return function(r){return function(e){return ps(e.type)(t)?Aa(e.type)(t)(e.value):r(e)}}}}}},n3=e3()()();var bi=function(){return function(t){var r=se(t);return function(e){return function(n){return{type:r(e),value:n}}}}};var a3=function(t){return za("Data.Variant: pattern match failure ["+(t.type+"]"))},lb=function(){return function(){return function(){return function(t){return n3(t)(a3)}}}};var pb=function(){var t=Fs(jD);return function(r){return $s(t(r))}}();var Uht=typeof Array.from=="function",Hht=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",zht=typeof String.prototype.fromCodePoint=="function",Vht=typeof String.prototype.codePointAt=="function";var of={proof:function(t){return t},Coercible0:function(){}},dx=function(t){return t.proof};var Ga=void 0;var Ov=function(t){return t.toInt},bx=function(t){var r=Ov(t);return function(e){return r(Ga)}};var ja={toInt:function(t){return 8}},yx={Nat0:function(){return ja}},No={toInt:function(t){return 7}},Ax={Nat0:function(){return No}},Ro={toInt:function(t){return 6}},kx={Nat0:function(){return Ro}},ta={toInt:function(t){return 5}},gx={Nat0:function(){return ta}},Du={toInt:function(t){return 4}},Qu={Nat0:function(){return Du}},du={toInt:function(t){return 3}},yi={Nat0:function(){return du}},bu={toInt:function(t){return 2}},Ai={Nat0:function(){return bu}},yu={toInt:function(t){return 1}},ki={Nat0:function(){return yu}},sn={toInt:function(t){return 0}};var Oe=function(t){return function(){return function(r){var e=r.Nat1();return function(){return function(n){return{Nat0:function(){return e},Pos1:function(){return t}}}}}}};var Ku={Nat0:function(){return No},Nat1:function(){return ja}};var Yu={Nat0:function(){return Ro},Nat1:function(){return ja}};var Zu={Nat0:function(){return ta},Nat1:function(){return ja}};var to={Nat0:function(){return Du},Nat1:function(){return ja}};var Lo={Nat0:function(){return Du},Nat1:function(){return ta}};var ro={Nat0:function(){return du},Nat1:function(){return ja}};var Bo={Nat0:function(){return du},Nat1:function(){return ta}};var eo={Nat0:function(){return bu},Nat1:function(){return ja}};var Wo={Nat0:function(){return bu},Nat1:function(){return ta}};var no={Nat0:function(){return yu},Nat1:function(){return ja}};var qo={Nat0:function(){return yu},Nat1:function(){return ta}};var ao={Nat0:function(){return sn},Nat1:function(){return ja}};var Uo={Nat0:function(){return sn},Nat1:function(){return ta}};var hx={Nat0:function(){return sn},Nat1:function(){return ja}};var sb={Nat0:function(){return sn},Nat1:function(){return No}};var vb={Nat0:function(){return sn},Nat1:function(){return Ro}};var mb={Nat0:function(){return sn},Nat1:function(){return ta}};var gi={Nat0:function(){return sn},Nat1:function(){return Du}};var Xa={Nat0:function(){return sn},Nat1:function(){return du}};var Qa={Nat0:function(){return sn},Nat1:function(){return bu}};var Ka={Nat0:function(){return sn},Nat1:function(){return yu}},hi={Nat0:function(){return sn},Nat1:function(){return sn}};var h3=Nc(),C3=A(Ar);var Cx=Do;var wv=function(t){return t};var db=function(t){var r=Ov(t);return function(){return function(e){return function(n){return h3(e)(r(n))}}}};var bb=function(t){var r=bx(t);return function(e){var n=r(D.value),a=function(){return n===0?[]:Ge(0)(n-1|0)}();return C3(e)(a)}};var Au=[];var Ne=function(t){return function(r){return function(e){return Hi(r)(e)}}};var ff={first:function(t){return function(r){return new G(t(r.value0),r.value1)}},second:A(vo),Profunctor0:function(){return Fn}},ul=function(t){return t.second},Mv=function(t){return t.first};var Ex=M();var R3=function(t){return function(r){return function(e){var n=Oo(e);return function(a){return n(t)(r)(a)}}}};var Fx=function(){return function(){return function(t){return R3(Ex)(Ex)(t)}}};var B3=Fx()(),$x=function(){return function(){return function(t){return B3(t)}}};var q3=function(t){return function(r){var e=Oo(r.Profunctor0()),n=Mv(r);return function(a){return e(t)(function(u){return u.value1(u.value0)})(n(a))}}},Ox=function(t){return function(r){return function(e){return q3(function(n){return new G(t(n),function(a){return r(n)(a)})})(e)}}};var wx=function(t){var r=To(t)(),e=Kh(t)()();return function(){return function(){return function(n){return function(a){return Ox(r(n))(lr(e(n)))(a)}}}}};var Mx=function(t){return t};var G3=JSON.parse;var j3=JSON.stringify;var Q3=A(Uf);var Iv=function(t){return t};var Pv=function(t){return t};var Nv=function(t){return function(r){return t(r)}},c_=function(t){var r=A(t);return{map:function(e){return Nv(r(Q3(e)))}}};var Ab=function(t){return{Applicative0:function(){return l_(t)},Bind1:function(){return kb(t)}}},kb=function(t){var r=yt(t.Bind1()),e=C(t.Applicative0());return{bind:function(n){return function(a){return r(n)(la(function(u){return e(Ht.create(u))})(function(u){var i=a(u);return i}))}},Apply0:function(){return Px(t)}}},Px=function(t){var r=c_(t.Bind1().Apply0().Functor0());return{apply:Na(Ab(t)),Functor0:function(){return r}}},l_=function(t){return{pure:function(){var r=C(t.Applicative0());return function(e){return Iv(r(zt.create(e)))}}(),Apply0:function(){return Px(t)}}};var Nx=function(t){var r=Ab(t);return{throwError:function(){var e=C(t.Applicative0());return function(n){return Iv(e(Ht.create(n)))}}(),Monad0:function(){return r}}};var gb=function(t){var r=vt(t);return function(e){var n=e.Bind1(),a=yt(n),u=C(e.Applicative0()),i=c_(n.Apply0().Functor0());return{alt:function(o){return function(f){return a(o)(function(p){if(p instanceof zt)return u(new zt(p.value0));if(p instanceof Ht)return a(f)(function(l){if(l instanceof zt)return u(new zt(l.value0));if(l instanceof Ht)return u(new Ht(r(p.value0)(l.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[l.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[p.constructor.name])})}},Functor0:function(){return i}}}};var Z3=he();var hb=function(t){return Z3(Pv(t))};function p_(t){return Object.prototype.toString.call(t).slice(8,-1)}var Lx=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var Tb=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Bx=et;var Sb=function(t){var r=Xu(Nx(t));return function(e){return r(pb(e))}};var Eb=function(t){var r=C(l_(t)),e=Sb(t);return function(n){return function(a){if(p_(a)===n)return r(Bx(a));if(Yr)return e(new Tb(n,p_(a)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[n.constructor.name,a.constructor.name])}}};var Fb=function(t){return Eb(t)("String")};var v2=wx({reflectSymbol:function(){return"o"}})()();var m2={reflectSymbol:function(){return"2x"}};var D2={reflectSymbol:function(){return"on"}},d2={reflectSymbol:function(){return"off"}};var b2=$x()()(Fn);var ku=bi(),y2=C(nt),A2=ui(),k2=ku({reflectSymbol:function(){return"onOff"}});var Bv=function(){function t(){}return t.value=new t,t}(),Wv=function(){function t(){}return t.value=new t,t}(),Ux=function(){function t(){}return t.value=new t,t}(),Hx=function(){function t(){}return t.value=new t,t}(),Ob=function(){function t(){}return t.value=new t,t}(),zx=function(){function t(){}return t.value=new t,t}(),Vx=function(){function t(){}return t.value=new t,t}();var Jx=function(t){return t},Gx=function(t){return t};var jx=function(t){return t};var Xx=function(t){return t};var Qx=function(t){return t};var Kx=function(t){return t},Yx=function(t){return t},Zx=function(t){return t},tT=function(t){return t},rT=function(t){return t};var wb=function(){function t(){}return t.value=new t,t}(),eT=function(){function t(){}return t.value=new t,t}(),nT=function(){function t(){}return t.value=new t,t}(),Mb=function(){function t(){}return t.value=new t,t}(),aT=function(){function t(){}return t.value=new t,t}();var qv=function(t){return t};var il=function(t){return t};var g2=function(t){return t},__=function(t){return t};var lc={toAudioOnOff:ot(it)};var pc=function(t){return t.toAudioParameter},uT=function(t){return t.toAudioOnOff},oT=function(){return zc.create}(),iT=function(){return tc.value}();var Uv=function(){return Mx(function(){var t=v2(D.value)(ff);return function(r){return b2(t(r))}}())},fT=et;var h2=function(){var t=ku({reflectSymbol:function(){return"unit"}})(D.value);return function(r){return __(t(r))}}();var C2=function(t){return function(r){return{toAudioParameter:function(e){return h2(e)}}}},cT=function(t){var r=C2(t);return function(e){return{toAudioParameter:function(){var n=pc(r(e));return function(a){return n(g2(function(u){return{u}}(a)))}}()}}},lT=function(){return ku(m2)(D.value)(void 0)}(),pT=function(){var t=ku({reflectSymbol:function(){return"sudden"}})(D.value);return function(r){return __(t(r))}}();var _T={toAudioParameter:pT},Hv={toAudioParameter:function(t){return pT({n:t})}},Ib=function(){return ku({reflectSymbol:function(){return"step"}})(D.value)(void 0)}();var Pb=function(){return ku(D2)(D.value)(void 0)}(),s_={x:Pb,o:0},at=function(){return y2(A2(k2(D.value)(s_)))};var sT=function(){return ku(d2)(D.value)(void 0)}();var x2=function(){var t=ku({reflectSymbol:function(){return"numeric"}})(D.value);return function(r){return __(t(r))}}();var gu={toAudioParameter:x2};var Ho=function(){return ku({reflectSymbol:function(){return"linear"}})(D.value)(void 0)}();var vT=function(){return ku({reflectSymbol:function(){return"exponential"}})(D.value)(void 0)}(),T2=function(){var t=ku({reflectSymbol:function(){return"envelope"}})(D.value);return function(r){return __(t(r))}}();var wn={toAudioParameter:T2},S2=function(){var t=ku({reflectSymbol:function(){return"cancel"}})(D.value);return function(r){return __(t(r))}}();var mT={toAudioParameter:S2};var E2=bi(),F2=E2({reflectSymbol:function(){return"realImg"}}),de=ot(it),DT=dx(of),dT=M();var cf=wr(),lf=St(Or),pf=$r()(),bT={reflectSymbol:function(){return"buffer"}},zv={reflectSymbol:function(){return"frequency"}},$2=function(){function t(){}return t.value=new t,t}(),O2=function(){function t(){}return t.value=new t,t}(),w2=function(){function t(){}return t.value=new t,t}(),M2=function(){function t(){}return t.value=new t,t}(),I2=function(){function t(){}return t.value=new t,t}(),P2=function(){function t(){}return t.value=new t,t}(),N2=function(){function t(){}return t.value=new t,t}(),R2=function(){function t(){}return t.value=new t,t}(),L2=function(){function t(){}return t.value=new t,t}(),B2=function(){function t(){}return t.value=new t,t}(),W2=function(){function t(){}return t.value=new t,t}(),q2=function(){function t(){}return t.value=new t,t}(),U2=function(){function t(){}return t.value=new t,t}(),H2=function(){function t(){}return t.value=new t,t}(),fl=function(t){return{toPeriodicOscSpec:function(r){return F2(D.value)({real:wv(r.value0),img:wv(r.value1)})}}};var Vv={toInitializeTriangleOsc:function(t){return rT(function(r){return{frequency:r}}(t))}};var yT={toInitializeStereoPanner:function(t){return tT(function(r){return{pan:r}}(t))}};var cl={toInitializeSquareOsc:function(t){return Zx(function(r){return{frequency:r}}(t))}};var _f={toInitializeSinOsc:function(t){return Yx(function(r){return{frequency:r}}(t))}};var AT={toInitializeSawtoothOsc:function(t){return Kx(function(r){return{frequency:r}}(t))}};var Nb={toInitializeRecorder:function(t){return Jx(function(r){return{cb:r}}(t))}};var v_={toInitializeMicrophone:function(t){return Gx(function(r){return{microphone:r}}(t))}};var kT=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:DT(dT(e.value0)),feedback:DT(dT(e.value1))}}}}}}};var dt={toInitializeGain:function(t){return Qx(function(r){return{gain:r}}(t))}};var gT={toInitializeConvolver:function(t){return jx(function(r){return{buffer:r}}(t))}},Jv={toInitializeConstant:function(t){return Xx(function(r){return{offset:r}}(t))}};var z2={convertOption:function(t){return function(r){return de}}},m_={convertOption:function(t){return function(r){return de}}},hT={convertOption:function(t){return function(r){return de}}},CT={convertOption:function(t){return function(r){return h.create}}},xT={convertOption:function(t){return function(r){return de}}},ll={convertOption:function(t){return function(r){return de}}},TT={convertOption:function(t){return function(r){return de}}},ST={convertOption:function(t){return function(r){return de}}},ET={convertOption:function(t){return function(r){return de}}},FT={convertOption:function(t){return function(r){return de}}},$T={convertOption:function(t){return function(r){return de}}},OT={convertOption:function(t){return function(r){return de}}},wT={convertOption:function(t){return function(r){return de}}},MT={convertOption:function(t){return function(r){return de}}},Rb={convertOption:function(t){return function(r){return de}}},pl={convertOption:function(t){return function(r){return de}}},Gv={convertOption:function(t){return function(r){return de}}},jv={convertOption:function(t){return function(r){return de}}};var Xv={convertOption:function(t){return function(r){return de}}},IT={convertOption:function(t){return function(r){return de}}},PT={convertOption:function(t){return function(r){return de}}},NT={convertOption:function(t){return function(r){return de}}},Lb={convertOption:function(t){return function(r){return de}}};var RT={convertOption:function(t){return function(r){return de}}},Bb={convertOption:function(t){return function(r){return de}}},zo={convertOption:function(t){return function(r){return de}}},uo={convertOption:function(t){return function(r){return de}}},LT={convertOption:function(t){return function(r){return de}}},Wb={convertOption:function(t){return function(r){return de}}},V2=function(t){return t.toPeriodicOscSpec},_l=function(t){var r=V2(t);return{convertOption:function(e){return function(n){return r}}}},qb=function(t){return t.toInitializeWaveShaper},BT=function(t){return t.toInitializeTriangleOsc},WT=function(t){return t.toInitializeStereoPanner},qT=function(t){return t.toInitializeSquareOsc},UT=function(t){return t.toInitializeSinOsc},HT=function(t){return t.toInitializeSawtoothOsc},zT=function(t){return t.toInitializeRecorder},Ub=function(t){return t.toInitializePlayBuf},VT=function(t){return t.toInitializePeriodicOsc},JT=function(t){return t.toInitializePeaking},GT=function(t){return t.toInitializeNotch},jT=function(t){return t.toInitializeMicrophone},XT=function(t){return t.toInitializeLowshelf},Hb=function(t){return t.toInitializeLowpass},zb=function(t){return t.toInitializeLoopBuf},QT=function(t){return t.toInitializeIIRFilter},KT=function(t){return t.toInitializeHighshelf},Vb=function(t){return t.toInitializeHighpass},YT=function(t){return t.toInitializeGain},ZT=function(t){return t.toInitializeDynamicsCompressor},Jb=function(t){return t.toInitializeDelay},tS=function(t){return t.toInitializeConvolver},rS=function(t){return t.toInitializeConstant},Gb=function(t){return t.toInitializeBandpass},jb=function(t){return t.toInitializeAllpass};var J2={oversample:lT},G2=function(t){var r=Jn(t);return{toInitializeWaveShaper:function(e){return r($2.value)(J2)(e)}}},eS={toInitializeWaveShaper:function(){var t=qb(G2(ar(cf(lf(z2)()()()({reflectSymbol:function(){return"curve"}})))(pf)));return function(r){return t(function(e){return{curve:e}}(r))}}()},j2=function(){return{bufferOffset:0,playbackRate:1,duration:F.value}}(),D_=function(t){var r=Jn(t);return{toInitializePlayBuf:function(e){return r(O2.value)(j2)(e)}}},hu={toInitializePlayBuf:function(){var t=Ub(D_(ar(cf(lf(m_)()()()(bT)))(pf)));return function(r){return t(function(e){return{buffer:e}}(r))}}()},X2={},sl=function(t){var r=Jn(t);return{toInitializePeriodicOsc:function(e){return r(w2.value)(X2)(e)}}},Q2={q:1,gain:0},nS=function(t){var r=Jn(t);return{toInitializePeaking:function(e){return r(M2.value)(Q2)(e)}}};var K2={q:1},aS=function(t){var r=Jn(t);return{toInitializeNotch:function(e){return r(I2.value)(K2)(e)}}};var Y2={gain:0},uS=function(t){var r=Jn(t);return{toInitializeLowshelf:function(e){return r(P2.value)(Y2)(e)}}};var Z2={q:1},Xb=function(t){var r=Jn(t);return{toInitializeLowpass:function(e){return r(N2.value)(Z2)(e)}}},Qv={toInitializeLowpass:function(){var t=Hb(Xb(ar(cf(lf(Rb)()()()(zv)))(pf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},tW=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:F.value}}(),vl=function(t){var r=Jn(t);return{toInitializeLoopBuf:function(e){return r(R2.value)(tW)(e)}}},Dr={toInitializeLoopBuf:function(){var t=zb(vl(ar(cf(lf(pl)()()()(bT)))(pf)));return function(r){return t(function(e){return{buffer:e}}(r))}}()},rW={gain:0},oS=function(t){var r=Jn(t);return{toInitializeHighshelf:function(e){return r(L2.value)(rW)(e)}}};var eW={q:1},Qb=function(t){var r=Jn(t);return{toInitializeHighpass:function(e){return r(B2.value)(eW)(e)}}},ml={toInitializeHighpass:function(){var t=Vb(Qb(ar(cf(lf(Lb)()()()(zv)))(pf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},nW=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),iS=function(t){var r=Jn(t);return{toInitializeDynamicsCompressor:function(e){return r(W2.value)(nW)(e)}}},aW={maxDelayTime:1},Kb=function(t){var r=Jn(t);return{toInitializeDelay:function(e){return r(q2.value)(aW)(e)}}},oo={toInitializeDelay:function(){var t=Jb(Kb(ar(cf(lf(Bb)()()()({reflectSymbol:function(){return"delayTime"}})))(pf)));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},uW={q:1},io=function(t){var r=Jn(t);return{toInitializeBandpass:function(e){return r(U2.value)(uW)(e)}}},fS={toInitializeBandpass:function(){var t=Gb(io(ar(cf(lf(uo)()()()(zv)))(pf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()},oW={q:1},Yb=function(t){var r=Jn(t);return{toInitializeAllpass:function(e){return r(H2.value)(oW)(e)}}},cS={toInitializeAllpass:function(){var t=jb(Yb(ar(cf(lf(Wb)()()()(zv)))(pf)));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var cW=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},_S=ot(it);var Kv=he(),b_=M();var lS=C(Yt),kr=C(nt),kn=C(je),we=me(Gt)(T),un=A(Ct),Ye=lb()()(),ma=w(T),pS=Gs(),d_=bi(),lW=$t(jt),pW=Wr(pa),vn=Co($e),_W=wd()()(),sS=Ta(Fn),sW=A(Lp),vW=yd()()()()()({reflectType:function(){return 0}});var mW=function(){function t(){}return t.value=new t,t}();var Yv={convertOption:function(t){return function(r){return _S}}},Zv={convertOption:function(t){return function(r){return _S}}};var DW=function(t){return t.toInitializeAnalyser},Fe=function(t){if(t instanceof Ip)return F.value;if(t instanceof So)return new h(t.value0);throw new Error("Failed pattern match at Ocarina.Control (line 38, column 1 - line 38, column 38): "+[t.constructor.name])},Sa=O0({doLogic:Ni,ids:function(t){return function(r){return r.ids}(Kv(t))},disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:b_,connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var dW=function(){return{cb:function(t){return lS(lS(void 0))},fftSize:Ob.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:Mb.value,channelInterpretation:wb.value}}(),tm=function(t){var r=Jn(t);return{toInitializeAnalyser:function(e){return r(mW.value)(dW)(e)}}};var bW=function(t){var r=jT(t);return function(e){var n=r(e),a=function(u){return function(i){return hr(function(o,f){var p=i.ids();u.raiseId(p)();var l=o(kr(i.makeMicrophone({id:p,parent:u.parent,scope:Fe(u.scope),microphone:n.microphone})),f);return function(){return f(i.deleteFromCache({id:p})),l()}})}};return new S(a)}},y_=function(t){return bW(t)};var gn=xa({doLogic:Ni,ids:function(t){return function(r){return r.ids}(Kv(t))},disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),yW=function(t){var r=DW(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeAnalyser({id:_,parent:o.parent,scope:Fe(o.scope),cb:u.cb,fftSize:Ud(2)(function(){if(u.fftSize instanceof Bv)return 7;if(u.fftSize instanceof Wv)return 8;if(u.fftSize instanceof Ux)return 9;if(u.fftSize instanceof Hx)return 10;if(u.fftSize instanceof Ob)return 11;if(u.fftSize instanceof zx)return 12;if(u.fftSize instanceof Vx)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 198, column 27 - line 205, column 40): "+[u.fftSize.constructor.name])}()),maxDecibels:u.maxDecibels,minDecibels:u.minDecibels,smoothingTimeConstant:u.smoothingTimeConstant,channelCount:u.channelCount,channelCountMode:function(){if(u.channelCountMode instanceof aT)return"explicit";if(u.channelCountMode instanceof Mb)return"max";if(u.channelCountMode instanceof nT)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 211, column 41 - line 214, column 52): "+[u.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(u.channelInterpretation instanceof wb)return"speakers";if(u.channelInterpretation instanceof eT)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 215, column 46 - line 217, column 47): "+[u.channelInterpretation.constructor.name])}()})),un(function(s){return Ye({cb:function(c){return f.setAnalyserNodeCb({id:_,cb:c})}})(s)})(n),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},rm=function(t){var r=yW(t);return function(e){return r(e)(ma)}},vS=function(t){var r=tS(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeConvolver({id:l,parent:i.parent,scope:Fe(i.scope),buffer:a.buffer})),gn({parent:new h(l),scope:i.scope,raiseId:function(m){return kn(void 0)}})(o)(I(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},AW=function(){return function(){return function(t){var r=QT(t);return function(e){return function(n){return function(a){return function(u){var i=r(a)(e)(n),o=function(f){return function(p){return hr(function(l,_){var m=p.ids();f.raiseId(m)();var s=l(we([kr(p.makeIIRFilter({id:m,parent:f.parent,scope:Fe(f.scope),feedforward:pS(i.feedforward),feedback:pS(i.feedback)})),gn({parent:new h(m),scope:f.scope,raiseId:function(c){return kn(void 0)}})(p)(I(u))]),_);return function(){return _(p.deleteFromCache({id:m})),s()}})}};return new S(o)}}}}}}},kW=AW()(),mS=function(){return function(){return function(t){return kW(t)(D.value)(D.value)}}},Zb=function(t){var r=zT(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeRecorder({id:l,parent:i.parent,scope:Fe(i.scope),cb:a.cb})),gn({parent:new h(l),scope:i.scope,raiseId:function(m){return kn(void 0)}})(o)(n)]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},gW=function(t){return function(r){return hr(function(e,n){var a=r.ids();return n(r.makeSpeaker({id:a})),e(gn({parent:new h(a),scope:new So("toplevel"),raiseId:function(u){return kn(void 0)}})(r)(I(t)),n)})}},_c=gW,Nt=function(t){return function(r){return function(e){return Da(t)(r)(ma)(e)}}},Da=function(t){var r=YT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeGain({id:_,parent:o.parent,scope:Fe(o.scope),gain:u.gain})),vn(un(function(s){return Ye({gain:DS(639)(o.scope)(f)(function(c){return f.setGain(function(v){return{id:_,gain:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},DS=cW("tmpResolveAU","Ocarina.Control",function(){var t=function(){var i=d_({reflectSymbol:function(){return"unit"}})(D.value);return function(o){return il(i(o))}}(),r=function(){var i=d_({reflectSymbol:function(){return"sudden"}})(D.value);return function(o){return il(i(o))}}(),e=function(){var i=d_({reflectSymbol:function(){return"numeric"}})(D.value);return function(o){return il(i(o))}}(),n=function(){var i=d_({reflectSymbol:function(){return"envelope"}})(D.value);return function(o){return il(i(o))}}(),a=function(){var i=d_({reflectSymbol:function(){return"cancel"}})(D.value);return function(o){return il(i(o))}}(),u=function(i){return function(o){return function(f){return function(p){return Ye({numeric:function(l){return kr(f(e(l)))},envelope:function(l){return kr(f(n(l)))},cancel:function(l){return kr(f(a(l)))},sudden:function(l){return kr(f(r(l)))},unit:function(l){var _=Nt(dt)(1)([l.u]);return hr(function(m,s){var c=We(F.value)();return m(lW(gn({parent:F.value,scope:i,raiseId:function(v){return pW(Wa(new h(v))(c))}})(o)(_))(hr(function(v,K){return function(){var xt=pn(c)();if(xt instanceof F)return void 0;if(xt instanceof h)return K(f(t({i:xt.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1831, column 42 - line 1833, column 80): "+[xt.constructor.name])}(),kn(void 0)})),s)})}})(p)}}}};return u}),ne=DS(1808),hW=Nt(dt),CW=function(t){var r=zb(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeLoopBuf({id:l,parent:i.parent,scope:Fe(i.scope),buffer:a.buffer,playbackRate:a.playbackRate,loopStart:a.loopStart,loopEnd:a.loopEnd,duration:a.duration})),vn(un(function(m){return Ye({buffer:function(s){return kr(o.setBuffer({id:l,buffer:s}))},playbackRate:ne(i.scope)(o)(function(s){return o.setPlaybackRate(function(c){return{id:l,playbackRate:c}}(s))}),loopStart:function(s){return kr(o.setLoopStart({id:l,loopStart:s}))},loopEnd:function(s){return kr(o.setLoopEnd({id:l,loopEnd:s}))},onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},ur=function(t){return CW(t)};var xW=function(t){var r=VT(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makePeriodicOsc({id:l,parent:i.parent,scope:Fe(i.scope),frequency:a.frequency,spec:a.spec})),vn(un(function(m){return Ye({frequency:ne(i.scope)(o)(function(s){return o.setFrequency(function(c){return{id:l,frequency:c}}(s))}),onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))},spec:function(s){return kr(o.setPeriodicOsc({id:l,spec:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},Dl=function(t){return xW(t)};var TW=function(t){var r=Ub(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makePlayBuf({id:l,parent:i.parent,scope:Fe(i.scope),buffer:a.buffer,playbackRate:a.playbackRate,bufferOffset:a.bufferOffset,duration:a.duration})),vn(un(function(m){return Ye({buffer:function(s){return kr(o.setBuffer({id:l,buffer:s}))},playbackRate:ne(i.scope)(o)(function(s){return o.setPlaybackRate(function(c){return{id:l,playbackRate:c}}(s))}),bufferOffset:function(s){return kr(o.setBufferOffset({id:l,bufferOffset:s}))},onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))},duration:function(s){return kr(o.setDuration({id:l,duration:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},ra=function(t){return TW(t)};var SW=function(t){var r=HT(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeSawtoothOsc({id:l,parent:i.parent,scope:Fe(i.scope),frequency:a.frequency})),vn(un(function(m){return Ye({frequency:ne(i.scope)(o)(function(s){return o.setFrequency(function(c){return{id:l,frequency:c}}(s))}),onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},dS=function(t){return SW(t)};var EW=function(t){var r=UT(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeSinOsc({id:l,parent:i.parent,scope:Fe(i.scope),frequency:a.frequency})),vn(un(function(m){return Ye({frequency:ne(i.scope)(o)(function(s){return o.setFrequency(function(c){return{id:l,frequency:c}}(s))}),onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},sc=function(t){return EW(t)},bS=function(t){var r=sc(t);return function(e){return r(e)(ma)}},FW=function(t){var r=qT(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeSquareOsc({id:l,parent:i.parent,scope:Fe(i.scope),frequency:a.frequency})),vn(un(function(m){return Ye({frequency:ne(i.scope)(o)(function(s){return o.setFrequency(function(c){return{id:l,frequency:c}}(s))}),onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},A_=function(t){return FW(t)},yS=function(t){var r=A_(t);return function(e){return r(e)(ma)}},$W=function(t){var r=BT(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeTriangleOsc({id:l,parent:i.parent,scope:Fe(i.scope),frequency:a.frequency})),vn(un(function(m){return Ye({frequency:ne(i.scope)(o)(function(s){return o.setFrequency(function(c){return{id:l,frequency:c}}(s))}),onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},em=function(t){return $W(t)};var OW=function(t){var r=jb(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeAllpass({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,q:u.q})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),q:ne(o.scope)(f)(function(c){return f.setQ(function(v){return{id:_,q:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},ty=function(t){var r=OW(t);return function(e){return function(n){return r(e)(ma)(n)}}},ry=function(t){var r=Gb(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeBandpass({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,q:u.q})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),q:ne(o.scope)(f)(function(c){return f.setQ(function(v){return{id:_,q:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},Vo=function(t){var r=ry(t);return function(e){return function(n){return r(e)(ma)(n)}}},k_=function(t){var r=Jb(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeDelay({id:_,parent:o.parent,scope:Fe(o.scope),delayTime:u.delayTime,maxDelayTime:u.maxDelayTime})),vn(un(function(s){return Ye({delayTime:ne(o.scope)(f)(function(c){return f.setDelay(function(v){return{id:_,delayTime:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},sf=function(t){var r=k_(t);return function(e){return function(n){return r(e)(ma)(n)}}},wW=function(t){var r=ZT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeDynamicsCompressor({id:_,parent:o.parent,scope:Fe(o.scope),threshold:u.threshold,ratio:u.ratio,knee:u.knee,attack:u.attack,release:u.release})),vn(un(function(s){return Ye({threshold:ne(o.scope)(f)(function(c){return f.setThreshold(function(v){return{id:_,threshold:v}}(c))}),ratio:ne(o.scope)(f)(function(c){return f.setRatio(function(v){return{id:_,ratio:v}}(c))}),knee:ne(o.scope)(f)(function(c){return f.setKnee(function(v){return{id:_,knee:v}}(c))}),attack:ne(o.scope)(f)(function(c){return f.setAttack(function(v){return{id:_,attack:v}}(c))}),release:ne(o.scope)(f)(function(c){return f.setRelease(function(v){return{id:_,release:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},AS=function(t){var r=wW(t);return function(e){return r(e)(ma)}},MW=function(){return function(t){return function(r){return _W({doLogic:Ni,ids:function(e){return function(n){return n.ids}(Kv(e))},disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:b_,fromEltO2:b_,toElt:b_,wrapElt:function(e){return hW(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(e){return function(n){return n.deleteFromCache}(Kv(e))}})(t)(sS(sW(function(e){return e(void 0)}))(b_(r)))}}},IW=MW(),ea=function(t){return function(r){return IW(bd(t))(sS(vW(D.value))(r))}};var ey=function(t){var r=Vb(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeHighpass({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,q:u.q})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),q:ne(o.scope)(f)(function(c){return f.setQ(function(v){return{id:_,q:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},dl=function(t){var r=ey(t);return function(e){return function(n){return r(e)(ma)(n)}}},PW=function(t){var r=KT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeHighshelf({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,gain:u.gain})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),gain:ne(o.scope)(f)(function(c){return f.setGain(function(v){return{id:_,gain:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},kS=function(t){var r=PW(t);return function(e){return function(n){return r(e)(ma)(n)}}},gS=function(t){var r=Hb(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeLowpass({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,q:u.q})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),q:ne(o.scope)(f)(function(c){return f.setQ(function(v){return{id:_,q:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},bl=function(t){var r=gS(t);return function(e){return function(n){return r(e)(ma)(n)}}},NW=function(t){var r=XT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeLowshelf({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,gain:u.gain})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),gain:ne(o.scope)(f)(function(c){return f.setGain(function(v){return{id:_,gain:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},hS=function(t){var r=NW(t);return function(e){return function(n){return r(e)(ma)(n)}}},RW=function(t){var r=GT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeNotch({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,q:u.q})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),q:ne(o.scope)(f)(function(c){return f.setQ(function(v){return{id:_,q:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},CS=function(t){var r=RW(t);return function(e){return function(n){return r(e)(ma)(n)}}},LW=function(t){var r=WT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makeStereoPanner({id:_,parent:o.parent,scope:Fe(o.scope),pan:u.pan})),vn(un(function(s){return Ye({pan:ne(o.scope)(f)(function(c){return f.setPan(function(v){return{id:_,pan:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},xS=function(t){var r=LW(t);return function(e){return r(e)(ma)}},BW=function(t){var r=JT(t);return function(e){return function(n){return function(a){var u=r(e),i=function(o){return function(f){return hr(function(p,l){var _=f.ids();o.raiseId(_)();var m=p(we([kr(f.makePeaking({id:_,parent:o.parent,scope:Fe(o.scope),frequency:u.frequency,q:u.q,gain:u.gain})),vn(un(function(s){return Ye({frequency:ne(o.scope)(f)(function(c){return f.setFrequency(function(v){return{id:_,frequency:v}}(c))}),q:ne(o.scope)(f)(function(c){return f.setQ(function(v){return{id:_,q:v}}(c))}),gain:ne(o.scope)(f)(function(c){return f.setGain(function(v){return{id:_,gain:v}}(c))})})(s)})(n)),gn({parent:new h(_),scope:o.scope,raiseId:function(s){return kn(void 0)}})(f)(I(a))]),l);return function(){return l(f.deleteFromCache({id:_})),m()}})}};return new S(i)}}}},TS=function(t){var r=BW(t);return function(e){return function(n){return r(e)(ma)(n)}}},SS=function(t){var r=qb(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeWaveShaper({id:l,parent:i.parent,scope:Fe(i.scope),curve:a.curve,oversample:a.oversample})),gn({parent:new h(l),scope:i.scope,raiseId:function(m){return kn(void 0)}})(o)(I(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},WW=function(t){var r=rS(t);return function(e){return function(n){var a=r(e),u=function(i){return function(o){return hr(function(f,p){var l=o.ids();i.raiseId(l)();var _=f(we([kr(o.makeConstant({id:l,parent:i.parent,scope:Fe(i.scope),offset:a.offset})),vn(un(function(m){return Ye({offset:ne(i.scope)(o)(function(s){return o.setOffset(function(c){return{id:l,offset:c}}(s))}),onOff:function(s){return kr(o.setOnOff({id:l,onOff:s}))}})(m)})(n))]),p);return function(){return p(o.deleteFromCache({id:l})),_()}})}};return new S(u)}}},nm=function(t){return WW(t)};function ny(){window.scrollTo(0,0)}var fo=function(t){return t.sequential},Ya=function(t){return t.parallel};var ES=M(),on=function(t){return function(r){return new S(V("button")(t)(ES(I(ES(U(z)(r))))))}};var na=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",i="Bind",o="Bracket",f="Fork",p="Sequential",l="Map",_="Apply",m="Alt",s="Cons",c="Resume",v="Release",K="Finalizer",pt="Finalized",xt="Forked",xr="Fiber",ce="Thunk";function Z(wt,Qr,Re,ae){this.tag=wt,this._1=Qr,this._2=Re,this._3=ae}function Xt(wt){var Qr=function(Re,ae,Qt){return new Z(wt,Re,ae,Qt)};return Qr.tag=wt,Qr}function At(wt){return new Z(r,void 0)}function jr(wt){try{wt()}catch(Qr){setTimeout(function(){throw Qr},0)}}function Jr(wt,Qr,Re){try{return Qr(Re())}catch(ae){return wt(ae)}}function fn(wt,Qr,Re){try{return Qr(Re)()}catch(ae){return Re(wt(ae))(),At}}var In=function(){var wt=1024,Qr=0,Re=0,ae=new Array(wt),Qt=!1;function _t(){var Zr;for(Qt=!0;Qr!==0;)Qr--,Zr=ae[Re],ae[Re]=void 0,Re=(Re+1)%wt,Zr();Qt=!1}return{isDraining:function(){return Qt},enqueue:function(Zr){var Fr,Me;Qr===wt&&(Me=Qt,_t(),Qt=Me),ae[(Re+Qr)%wt]=Zr,Qr++,Qt||_t()}}}();function nu(wt){var Qr={},Re=0,ae=0;return{register:function(Qt){var _t=Re++;Qt.onComplete({rethrow:!0,handler:function(Zr){return function(){ae--,delete Qr[_t]}}})(),Qr[_t]=Qt,ae++},isEmpty:function(){return ae===0},killAll:function(Qt,_t){return function(){if(ae===0)return _t();var Zr=0,Fr={};function Me(_e){Fr[_e]=Qr[_e].kill(Qt,function(Ve){return function(){delete Fr[_e],Zr--,wt.isLeft(Ve)&&wt.fromLeft(Ve)&&setTimeout(function(){throw wt.fromLeft(Ve)},0),Zr===0&&_t()}})()}for(var Cn in Qr)Qr.hasOwnProperty(Cn)&&(Zr++,Me(Cn));return Qr={},Re=0,ae=0,function(_e){return new Z(a,function(){for(var Ve in Fr)Fr.hasOwnProperty(Ve)&&Fr[Ve]()})}}}}}var xe=0,pe=1,Pa=2,Ko=3,kc=4,ca=5,hn=6;function Pf(wt,Qr,Re){var ae=0,Qt=xe,_t=Re,Zr=null,Fr=null,Me=null,Cn=null,_e=null,Ve=0,Nf=0,au=null,Ii=!0;function Pi(Kt){for(var tr,Hr,Xr;;)switch(tr=null,Hr=null,Xr=null,Qt){case Pa:Qt=pe;try{_t=Me(_t),Cn===null?Me=null:(Me=Cn._1,Cn=Cn._2)}catch(Pn){Qt=ca,Zr=wt.left(Pn),_t=null}break;case Ko:wt.isLeft(_t)?(Qt=ca,Zr=_t,_t=null):Me===null?Qt=ca:(Qt=Pa,_t=wt.fromRight(_t));break;case pe:switch(_t.tag){case i:Me&&(Cn=new Z(s,Me,Cn)),Me=_t._2,Qt=pe,_t=_t._1;break;case r:Me===null?(Qt=ca,_t=wt.right(_t._1)):(Qt=Pa,_t=_t._1);break;case a:Qt=Ko,_t=Jr(wt.left,wt.right,_t._1);break;case u:Qt=kc,_t=fn(wt.left,_t._1,function(Pn){return function(){ae===Kt&&(ae++,In.enqueue(function(){ae===Kt+1&&(Qt=Ko,_t=Pn,Pi(ae))}))}});return;case e:Qt=ca,Zr=wt.left(_t._1),_t=null;break;case n:Me===null?_e=new Z(s,_t,_e,Fr):_e=new Z(s,_t,new Z(s,new Z(c,Me,Cn),_e,Fr),Fr),Me=null,Cn=null,Qt=pe,_t=_t._1;break;case o:Ve++,Me===null?_e=new Z(s,_t,_e,Fr):_e=new Z(s,_t,new Z(s,new Z(c,Me,Cn),_e,Fr),Fr),Me=null,Cn=null,Qt=pe,_t=_t._1;break;case f:Qt=Ko,tr=Pf(wt,Qr,_t._2),Qr&&Qr.register(tr),_t._1&&tr.run(),_t=wt.right(tr);break;case p:Qt=pe,_t=Yo(wt,Qr,_t._1);break}break;case ca:if(Me=null,Cn=null,_e===null)Qt=hn,_t=Fr||Zr||_t;else switch(tr=_e._3,Xr=_e._1,_e=_e._2,Xr.tag){case n:Fr&&Fr!==tr&&Ve===0?Qt=ca:Zr&&(Qt=pe,_t=Xr._2(wt.fromLeft(Zr)),Zr=null);break;case c:Fr&&Fr!==tr&&Ve===0||Zr?Qt=ca:(Me=Xr._1,Cn=Xr._2,Qt=Pa,_t=wt.fromRight(_t));break;case o:Ve--,Zr===null&&(Hr=wt.fromRight(_t),_e=new Z(s,new Z(v,Xr._2,Hr),_e,tr),(Fr===tr||Ve>0)&&(Qt=pe,_t=Xr._3(Hr)));break;case v:_e=new Z(s,new Z(pt,_t,Zr),_e,Fr),Qt=pe,Fr&&Fr!==tr&&Ve===0?_t=Xr._1.killed(wt.fromLeft(Fr))(Xr._2):Zr?_t=Xr._1.failed(wt.fromLeft(Zr))(Xr._2):_t=Xr._1.completed(wt.fromRight(_t))(Xr._2),Zr=null,Ve++;break;case K:Ve++,_e=new Z(s,new Z(pt,_t,Zr),_e,Fr),Qt=pe,_t=Xr._1;break;case pt:Ve--,Qt=ca,_t=Xr._1,Zr=Xr._2;break}break;case hn:for(var Be in au)au.hasOwnProperty(Be)&&(Ii=Ii&&au[Be].rethrow,jr(au[Be].handler(_t)));au=null,Fr&&Zr?setTimeout(function(){throw wt.fromLeft(Zr)},0):wt.isLeft(_t)&&Ii&&setTimeout(function(){if(Ii)throw wt.fromLeft(_t)},0);return;case xe:Qt=pe;break;case kc:return}}function Le(Kt){return function(){if(Qt===hn)return Ii=Ii&&Kt.rethrow,Kt.handler(_t)(),function(){};var tr=Nf++;return au=au||{},au[tr]=Kt,function(){au!==null&&delete au[tr]}}}function cr(Kt,tr){return function(){if(Qt===hn)return tr(wt.right(void 0))(),function(){};var Hr=Le({rethrow:!1,handler:function(){return tr(wt.right(void 0))}})();switch(Qt){case xe:Fr=wt.left(Kt),Qt=hn,_t=Fr,Pi(ae);break;case kc:Fr===null&&(Fr=wt.left(Kt)),Ve===0&&(Qt===kc&&(_e=new Z(s,new Z(K,_t(Kt)),_e,Fr)),Qt=ca,_t=null,Zr=null,Pi(++ae));break;default:Fr===null&&(Fr=wt.left(Kt)),Ve===0&&(Qt=ca,_t=null,Zr=null)}return Hr}}function Pr(Kt){return function(){var tr=Le({rethrow:!1,handler:Kt})();return Qt===xe&&Pi(ae),tr}}return{kill:cr,join:Pr,onComplete:Le,isSuspended:function(){return Qt===xe},run:function(){Qt===xe&&(In.isDraining()?Pi(ae):In.enqueue(function(){Pi(ae)}))}}}function cs(wt,Qr,Re,ae){var Qt=0,_t={},Zr=0,Fr={},Me=new Error("[ParAff] Early exit"),Cn=null,_e=t;function Ve(Le,cr,Pr){var Kt=cr,tr=null,Hr=null,Xr=0,Be={},Pn,zl;t:for(;;)switch(Pn=null,Kt.tag){case xt:if(Kt._3===t&&(Pn=_t[Kt._1],Be[Xr++]=Pn.kill(Le,function(Sw){return function(){Xr--,Xr===0&&Pr(Sw)()}})),tr===null)break t;Kt=tr._2,Hr===null?tr=null:(tr=Hr._1,Hr=Hr._2);break;case l:Kt=Kt._2;break;case _:case m:tr&&(Hr=new Z(s,tr,Hr)),tr=Kt,Kt=Kt._1;break}if(Xr===0)Pr(wt.right(void 0))();else for(zl=0,Pn=Xr;zl<Pn;zl++)Be[zl]=Be[zl]();return Be}function Nf(Le,cr,Pr){var Kt,tr,Hr,Xr,Be,Pn;wt.isLeft(Le)?(Kt=Le,tr=null):(tr=Le,Kt=null);t:for(;;){if(Hr=null,Xr=null,Be=null,Pn=null,Cn!==null)return;if(cr===null){ae(Kt||tr)();return}if(cr._3!==t)return;switch(cr.tag){case l:Kt===null?(cr._3=wt.right(cr._1(wt.fromRight(tr))),tr=cr._3):cr._3=Kt;break;case _:if(Hr=cr._1._3,Xr=cr._2._3,Kt){if(cr._3=Kt,Be=!0,Pn=Zr++,Fr[Pn]=Ve(Me,Kt===Hr?cr._2:cr._1,function(){return function(){delete Fr[Pn],Be?Be=!1:Pr===null?Nf(Kt,null,null):Nf(Kt,Pr._1,Pr._2)}}),Be){Be=!1;return}}else{if(Hr===t||Xr===t)return;tr=wt.right(wt.fromRight(Hr)(wt.fromRight(Xr))),cr._3=tr}break;case m:if(Hr=cr._1._3,Xr=cr._2._3,Hr===t&&wt.isLeft(Xr)||Xr===t&&wt.isLeft(Hr))return;if(Hr!==t&&wt.isLeft(Hr)&&Xr!==t&&wt.isLeft(Xr))Kt=tr===Hr?Xr:Hr,tr=null,cr._3=Kt;else if(cr._3=tr,Be=!0,Pn=Zr++,Fr[Pn]=Ve(Me,tr===Hr?cr._2:cr._1,function(){return function(){delete Fr[Pn],Be?Be=!1:Pr===null?Nf(tr,null,null):Nf(tr,Pr._1,Pr._2)}}),Be){Be=!1;return}break}Pr===null?cr=null:(cr=Pr._1,Pr=Pr._2)}}function au(Le){return function(cr){return function(){delete _t[Le._1],Le._3=cr,Nf(cr,Le._2._1,Le._2._2)}}}function Ii(){var Le=pe,cr=Re,Pr=null,Kt=null,tr,Hr;t:for(;;)switch(tr=null,Hr=null,Le){case pe:switch(cr.tag){case l:Pr&&(Kt=new Z(s,Pr,Kt)),Pr=new Z(l,cr._1,t,t),cr=cr._2;break;case _:Pr&&(Kt=new Z(s,Pr,Kt)),Pr=new Z(_,t,cr._2,t),cr=cr._1;break;case m:Pr&&(Kt=new Z(s,Pr,Kt)),Pr=new Z(m,t,cr._2,t),cr=cr._1;break;default:Hr=Qt++,Le=ca,tr=cr,cr=new Z(xt,Hr,new Z(s,Pr,Kt),t),tr=Pf(wt,Qr,tr),tr.onComplete({rethrow:!1,handler:au(cr)})(),_t[Hr]=tr,Qr&&Qr.register(tr)}break;case ca:if(Pr===null)break t;Pr._1===t?(Pr._1=cr,Le=pe,cr=Pr._2,Pr._2=t):(Pr._2=cr,cr=Pr,Kt===null?Pr=null:(Pr=Kt._1,Kt=Kt._2))}for(_e=cr,Hr=0;Hr<Qt;Hr++)_t[Hr].run()}function Pi(Le,cr){Cn=wt.left(Le);var Pr;for(var Kt in Fr)if(Fr.hasOwnProperty(Kt)){Pr=Fr[Kt];for(Kt in Pr)Pr.hasOwnProperty(Kt)&&Pr[Kt]()}Fr=null;var tr=Ve(Le,_e,cr);return function(Hr){return new Z(u,function(Xr){return function(){for(var Be in tr)tr.hasOwnProperty(Be)&&tr[Be]();return At}})}}return Ii(),function(Le){return new Z(u,function(cr){return function(){return Pi(Le,cr)}})}}function Yo(wt,Qr,Re){return new Z(u,function(ae){return function(){return cs(wt,Qr,Re,ae)}})}return Z.EMPTY=t,Z.Pure=Xt(r),Z.Throw=Xt(e),Z.Catch=Xt(n),Z.Sync=Xt(a),Z.Async=Xt(u),Z.Bind=Xt(i),Z.Bracket=Xt(o),Z.Fork=Xt(f),Z.Seq=Xt(p),Z.ParMap=Xt(l),Z.ParApply=Xt(_),Z.ParAlt=Xt(m),Z.Fiber=Pf,Z.Supervisor=nu,Z.Scheduler=In,Z.nonCanceler=At,Z}(),FS=na.Pure,jW=na.Throw;function $S(t){return function(r){return r.tag===na.Pure.tag?na.Pure(t(r._1)):na.Bind(r,function(e){return na.Pure(t(e))})}}function OS(t){return function(r){return na.Bind(t,r)}}var wS=na.Sync;function MS(t){return function(r){return na.ParMap(t,r)}}function IS(t){return function(r){return na.ParApply(t,r)}}function PS(t){return function(r){return na.ParAlt(t,r)}}var yl=na.Async;function NS(t,r){return function(){return na.Fiber(t,null,r)}}var XW=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return na.Async(function(a){return function(){var u=t(n,a(e()));return function(){return na.Sync(function(){return e(r(n,u))})}}})}}(),RS=na.Seq;var KW=ot(it),YW=function(t){var r=fo(t),e=dn(t.Applicative1()),n=Ya(t);return function(a){var u=e(a);return function(i){var o=u(function(f){return n(i(f))});return function(f){return r(o(f))}}}},LS=function(t){var r=fo(t),e=t.Applicative1(),n=Ya(t);return function(a){var u=bn(a)(e);return function(i){var o=u(function(f){return n(i(f))});return function(f){return r(o(f))}}}},BS=function(t){var r=YW(t);return function(e){return r(e)(KW)}};var qS=Pg()();var ZW=function(t){return t};var US=function(t){return t};var Al=function(t){return t.toDuration};var HS={fromDuration:qS(ZW)(function(t){return t*1e3}),toDuration:qS(US)(function(t){return t/1e3})};var zS=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},rq=C(Yt),VS=Wr(R),JS=A(R);var eq=function(t){return t};var gl={map:MS},Ci={map:$S};var nq=function(){var t=function(n){if(n instanceof zt)return n.value0;if(n instanceof Ht)return za("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof Ht)return n.value0;if(n instanceof zt)return za("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof Ht)return!0;if(n instanceof zt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:Ht.create,right:zt.create}}(),aq=function(t){return NS(nq,t)},co=function(t){return function(){var e=aq(t)();return e.run(),e}},Go=function(t){return VS(co(t))};var h_={apply:IS,Functor0:function(){return gl}};var uy={Applicative0:function(){return ua},Bind1:function(){return aa}},aa={bind:OS,Apply0:function(){return oy(0)}},ua={pure:FS,Apply0:function(){return oy(0)}},oy=zS("applyAff","Effect.Aff",function(){return{apply:Na(uy),Functor0:function(){return Ci}}}),GS=oy(73),jS=C(ua),uq=yt(aa);var Ze={liftEffect:wS,Monad0:function(){return uy}},ay=Ee(Ze),XS=function(t){return eq(g(ay(t)))},QS=function(t){return yl(function(r){return JS(XS)(t.join(r))})};var KS=function(t){return function(r){return uq(ay(r.isSuspended))(function(e){return e?ay(VS(r.kill(t,g(rq(void 0))))):yl(function(n){return JS(XS)(r.kill(t,n))})})}};var Za={parallel:et,sequential:RS,Monad0:function(){return uy},Applicative1:function(){return oq(0)}},oq=zS("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Ya(Za);return function(r){return t(jS(r))}}(),Apply0:function(){return h_}}});var iq=BS(Za)(Gt);var fq={append:function(t){return function(r){return function(e){return iq([t(e),r(e)])}}}};var cq=g(jS(void 0)),YS={mempty:cq,Semigroup0:function(){return fq}};var ZS={alt:PS,Functor0:function(){return gl}};var tE=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),C_=function(){function t(){}return t.value=new t,t}(),vc=function(){function t(){}return t.value=new t,t}(),x_=function(){function t(){}return t.value=new t,t}(),mc=function(){function t(){}return t.value=new t,t}(),T_=function(){function t(){}return t.value=new t,t}(),S_=function(){function t(){}return t.value=new t,t}(),rE=function(){function t(){}return t.value=new t,t}(),am=function(){function t(){}return t.value=new t,t}(),um=function(){function t(){}return t.value=new t,t}(),E_=function(){function t(){}return t.value=new t,t}(),F_=function(){function t(){}return t.value=new t,t}(),eE=function(){function t(){}return t.value=new t,t}(),hl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),iy=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var lq="numeric",pq="sudden",_q="unit",sq="cancel",vq="step",mq="linear",Dq="exponential",dq="envelope",nE=function(t,r,e,n){if(e.type===pq)t.value=e.value.n;else if(e.type===_q)r.id&&yq(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===lq)t[e.value.t.type===vq?"setValueAtTime":e.value.t.type===mq?"linearRampToValueAtTime":e.value.t.type===Dq?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===sq)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===dq){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},bq=function(t,r,e,n,a){return n[e]||(n[e]={}),nE(r.parameters.get(e),n[e],a,t)},Cu=function(t,r,e,n,a){return n[e]||(n[e]={}),nE(r[e],n[e],a,t)},be=function(t,r,e,n){let a=t("@fan@")(u=>u)(e);n.scopes[a]||(n.scopes[a]=[]),n.scopes[a].push(r),n.units[r].scope=a},ye=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},Ae=function(t,r,e,n){t()(a=>aE(r,a,n))(e)},aE=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function fy(t){return function(r){return function(){delete r.units[t.id]}}}function cy(t){return function(r){return function(){aE(t.from,t.to,r)}}}var yq=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function ly(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var py=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},_y=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),i=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:i,main:e.context.createGain(),se:u},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},sy=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},vy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},my=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new ConstantSourceNode(i,o)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Dy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},dy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},by=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},yy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Ay=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},ky=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},gy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},hy=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new AudioBufferSourceNode(i,o)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Cy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},xy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Ty=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var i=e.context.createMediaElementSource(a);return i};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Sy=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Ey=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Fy=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},$y=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var f={frequency:o.frequency,periodicWave:o.spec.type==="wave"?o.spec.value:vA(e.context)(o.spec.value.real)(o.spec.value.img)()},p=new OscillatorNode(i,f);return p},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Oy=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var f={loop:o.loop,buffer:o.buffer,playbackRate:o.playbackRate};return new AudioBufferSourceNode(i,f)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(i=>i)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},wy=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),i=new MediaRecorder(u.stream);a(i)(),i.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:i,main:e.context.createGain(),se:u},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},My=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Iy=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Py=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},Ny=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Ry=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},Ly=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)},By=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},be(t,n,r.scope,e),ye(n,e),Ae(t,n,r.parent,e)};function Wy(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function qy(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function Uy(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function Hy(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;bq(r,r.units[e].main,n,r.units[e].controllers,a)}}}var xu=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function zy(t){return function(r){return function(){var e=t.id,n=t.gain;Cu(r,r.units[e].main,"gain",r.units[e].controllers,n),xu(n,r.units[e],"gain")}}}function Vy(t){return function(r){return function(){var e=t.id,n=t.q;Cu(r,r.units[e].main,"Q",r.units[e].controllers,n),xu(n,r.units[e],"Q")}}}function Jy(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function Gy(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function jy(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function Xy(t){return function(r){return function(){var e=t.id,n=t.pan;Cu(r,r.units[e].main,"pan",r.units[e].controllers,n),xu(n,r.units[e],"pan")}}}function Qy(t){return function(r){return function(){var e=t.id,n=t.threshold;Cu(r,r.units[e].main,"threshold",r.units[e].controllers,n),xu(n,r.units[e],"threshold")}}}function Ky(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function Yy(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function Zy(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function tA(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function rA(t){return function(r){return function(){var e=t.id,n=t.release;Cu(r,r.units[e].main,"release",r.units[e].controllers,n),xu(n,r.units[e],"release")}}}function eA(t){return function(r){return function(){var e=t.id,n=t.offset;Cu(r,r.units[e].main,"offset",r.units[e].controllers,n),xu(n,r.units[e],"offset")}}}function nA(t){return function(r){return function(){var e=t.id,n=t.ratio;Cu(r,r.units[e].main,"ratio",r.units[e].controllers,n),xu(n,r.units[e],"ratio")}}}function aA(t){return function(r){return function(){var e=t.id,n=t.attack;Cu(r,r.units[e].main,"attack",r.units[e].controllers,n),xu(n,r.units[e],"attack")}}}function uA(t){return function(r){return function(){var e=t.id,n=t.knee;Cu(r,r.units[e].main,"knee",r.units[e].controllers,n),xu(n,r.units[e],"knee")}}}function oA(t){return function(r){return function(){var e=t.id,n=t.delayTime;Cu(r,r.units[e].main,"delayTime",r.units[e].controllers,n),xu(n,r.units[e],"delayTime")}}}function iA(t){return function(r){return function(){var e=t.id,n=t.playbackRate;Cu(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),xu(n,r.units[e],"playbackRate")}}}function fA(t){return function(r){return function(){var e=t.id,n=t.frequency;Cu(r,r.units[e].main,"frequency",r.units[e].controllers,n),xu(n,r.units[e],"frequency")}}}function cA(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?Aq(e)(n)(r)():n.x.type==="off"&&kq(e)(n)(r)()}}}var Aq=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},kq=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function lA(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function om(t){return function(){t.stop()}}function pA(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function _A(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function $_(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function sA(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var vA=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function vf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function mA(t){return function(){t.close()}}function DA(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function dA(t){return function(r){return function(){return t.decodeAudioData(r)}}}function bA(){return new(window.AudioContext||window.webkitAudioContext)}function yA(t){return function(){return t.state}}function O_(t){return function(){return t.currentTime}}function uE(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var Cq=ls(R),xq=pr(YS),Tq=ot(it),Sq=$t(gb(GD)(iu)),Eq=Eb(iu),Fq=A(c_(Nu)),$q=Fb(iu),Oq=yt(aa),wq=Ee(Ze),Mq=function(t){return function(r){return yl(function(e){return Cq(xq)(uE(r)(function(n){return e(Ht.create(t(n)))()})(function(n){return e(zt.create(n))()}))})}};var Iq=function(t){return la(function(r){return Io("Promise failed, couldn't extract JS Error or String")})(Tq)(hb(Sq(Eq("Error")(t))(Fq(Io)($q(t)))))},oE=Mq(Iq),im=function(t){return Oq(wq(t))(oE)};function AA(t){return function(){return URL.createObjectURL(t)}}var Wq=Rt(ga);var qq=xn(Kn);var Uq=uc(yv),Hq=Wr(pa),zq=Rr(Iu),Vq=yt(aa);var Jq=Nn(Yt),Gq=A(Ci),iE=C(ri);var fE=function(t){return function(r){return function(e){return lr(pA(t))(e)(function(){var n=qq(r);return function(a){return n(AA(a))}}())}}};var Dc=function(t){return{ids:function(){var e=pn(t)(),n=Wq(uf(Uq)({newSeed:rf(e),size:5}));return Hq(qu(zq(1))(t))(),n},deleteFromCache:fy,disconnectXFromY:ly,connectXToY:cy,makeAllpass:py(Ft),makeAnalyser:_y(Ft),makeAudioWorkletNode:sy(Ft),makeBandpass:vy(Ft),makeConstant:my(Ft),makeConvolver:Dy(Ft),makeDelay:dy(Ft),makeDynamicsCompressor:by(Ft),makeGain:yy(Ft),makeHighpass:Ay(Ft),makeHighshelf:ky(Ft),makeIIRFilter:gy(Ft),makeLoopBuf:hy(Ft),makeLowpass:Cy(Ft),makeLowshelf:xy(Ft),makeMediaElement:Ty(Ft),makeMicrophone:Sy(Ft),makeNotch:Ey(Ft),makePeaking:Fy(Ft),makePeriodicOsc:$y(Ft),makePlayBuf:Oy(Ft),makeRecorder:wy(Ft),makeSawtoothOsc:My(Ft),makeSinOsc:Iy(Ft),makeSpeaker:Py,makeSquareOsc:Ry(Ft),makeStereoPanner:Ny(Ft),makeTriangleOsc:Ly(Ft),makeWaveShaper:By(Ft),setAnalyserNodeCb:Wy,setMediaRecorderCb:qy,setWaveShaperCurve:Uy,setAudioWorkletParameter:Hy,setBuffer:Jy,setConvolverBuffer:Gy,setDuration:tA(Ft),setPeriodicOsc:jy,setOnOff:cA,setBufferOffset:Zy,setLoopStart:Ky,setLoopEnd:Yy,setRatio:nA,setOffset:eA,setAttack:aA,setGain:zy,setQ:Vy,setPan:Xy,setThreshold:Qy,setRelease:rA,setKnee:uA,setDelay:oA,setPlaybackRate:iA,setFrequency:fA}},lt=function(t){return function(r){return Vq(im(DA(r)))(function(){var e=dA(t);return function(n){return im(e(n))}}())}},w_=function(t){var r=Ee(t);return function(e){return r(yA(e))}},jq=w_(fe);var Mn=function(t){return Ee(t)(bA)},Tu=function(t){var r=Ee(t);return function(e){return r(sA(e))}},mn=function(t){var r=Ee(t);return function(e){return r(function(){var a=jq(e)();return Jq(a!=="closed")(mA(e))()})}},Xq=et,Qq=et,fm=function(t){return function(r){return Gq(function(e){return{microphone:function(){return t?iE(Xq(e)):F.value}(),camera:function(){return r?iE(Qq(e)):F.value}()}})(im(_A(t)(r)))}};var Kq=fo(Za),Yq=$t(ZS),cE=Ya(Za),cm=yt(aa),lE=Ee(Ze),gA=kt(Se),M_=or(vr),I_=A(Ct),kA=C(Yt),Zq=Mn(Ze),tU=Tu(Ze),rU=mn(fe),eU=Dt(yn),pE=$t(jt),_E=C(nt),nU=kt(Zs),jo=function(){function t(){}return t.value=new t,t}(),Xo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ru=function(){function t(){}return t.value=new t,t}(),tn=ny,xi=function(t){return Kq(Yq(cE(cm(QS(t))(lE)))(cE(KS(Io("We navigated away from the page"))(t))))},P_=function(t){var r=$t(t);return function(e){var n=C(e);return function(a){return function(u){return r(n(ru.value))(u)}}}},sE=P_(jt)(nt),oa=function(t){var r=$t(t),e=A(t.Functor0());return function(n){var a=C(n);return function(u){return function(i){return r(a(gA(ie.value)(Kr(g(i)))))(e(function(o){return gA(ie.value)(Kr(g(M_(o)(i))))})(e(function(o){return o.value0})(u)))}}}},lm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return I_(function(i){return gA(ie.value)(Kr(g(function(){if(i.value0 instanceof jo)return kA(void 0);if(i.value0 instanceof Xo)return M_(M_(i.value0.value0)(t(kA(void 0))))(r(ru.value));if(i.value0 instanceof ru)return function(){i.value1(),r(jo.value)();var f=co(cm(Zq)(function(p){return cm(tU(p))(function(l){return cm(e(p))(function(_){return lE(function(){var s=n(p)(_)(),c=M_(M_(s)(l))(rU(p));return r(new Xo(c))(),c})})})}))();return t(function(){return r(ru.value)(),Go(xi(f))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 57, column 21 - line 75, column 26): "+[i.value0.constructor.name])}())))})(eU(I_(G.create)(u))(pE(_E(kA(void 0)))(I_(function(i){return i.value0})(a))))}}}}}},ia=function(t){return function(r){return function(e){return function(){return t(e)(),r(new tE(e))()}}}},pm=function(t){return function(r){return function(e){return function(n){return function(a){return He(function(u){return function(i){var o=sE(r)(i);return jc(pE(_E(nU(It.value)("cursor: pointer;")))(lm(e)(u)(n)(a)(r)(o)))([Qe(I_(function(f){if(f instanceof ru)return t;if(f instanceof jo)return"\u23F3";if(f instanceof Xo)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 126, column 17 - line 129, column 35): "+[f.constructor.name])})(o))])}})}}}}},bt=function(t){return function(r){return function(e){return function(n){return He(function(a){return function(u){var i=sE(t)(u);return on(lm(r)(a)(e)(n)(t)(i))([Qe(I_(function(o){if(o instanceof ru)return"Turn on";if(o instanceof jo)return"Loading...";if(o instanceof Xo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 100, column 17 - line 103, column 42): "+[o.constructor.name])})(i))])}})}}}};var vE=Ju(Vu),aU=A(Ct),uU=Mn(fe),oU=A(R),iU=or(vr),fU=mn(fe),Cl=function(t){return function(r){return function(){var n=vf(t)(),a=vE(We(0))(),u=Pe(_c([new Fo(aU(function(i){return Pp.create(t0(i))})(r))])(Dc(a)))(function(i){return i(n)})();return u}}};var ut=function(t){return function(r){return function(){var n=vf(t)(),a=vE(We(0))(),u=Pe(_c(r)(Dc(a)))(function(i){return i(n)})();return u}}},_m=function(t){return function(){var e=uU();return oU(function(n){return iU(n)(fU(e))})(ut(e)(t))()}};var cU=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),lU=ur(Dr),pU=at(),_U=Nt(dt),mE=ty(cS),DE=ty(Yb(ar(wr()(St(St(Or)(LT)()()()({reflectSymbol:function(){return"q"}}))(Wb)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),sU=function(){return D.value}(),dE=function(t){return function(r){return function(e){return cU(D.value)(sU)({allpass:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([ea(lU(a)(pU))(function(u){return function(i){return _U(.2)([u,mE(700)([DE({frequency:990,q:20})([u]),mE(1110)([u,DE({frequency:2010,q:30})([u])])])])}})])}}))})}}};function Ti(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,i=t.BYTES_PER_ELEMENT,o=Math.min(u,n>>>0);if(a===null)return new t(e,o);var f=Math.min((u-o)/i,a);return new t(e,o,f)}}var mU=Ti(Uint8ClampedArray),DU=Ti(Uint32Array),dU=Ti(Uint16Array),bE=Ti(Uint8Array),bU=Ti(Int32Array),yU=Ti(Int16Array),AU=Ti(Int8Array),kU=Ti(Float32Array),gU=Ti(Float64Array);function yE(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var sm={create:bE,BinaryValue0:function(){}};var vm=function(t){return function(r){return function(){return yE(r)}}};var xl=Ga,Tl=Ga,Sl=Ga,Ea=Ga,Fa=Ga,$a=Ga,Oa=Ga,wa=Ga;function mm(t){return t|0}var EU=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},FU=Wr(R),$U=Nn(Yt),mf=Yn(function(t){return function(){var e=di(),n=Nr(!0)(),a=EU("fx","FRP.Event.Animate",function(){return FU(lr(hv)(e)(function(){var o=zr(n)();return $U(o)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),te(!1)(n)}});var hE=bb(ja),TA=Ne(Oe(gx)()(gi)()(mb)),SA=Ne(Oe(Qu)()(Xa)()(gi)),EA=Ne(Oe(yi)()(Qa)()(Xa)),FA=Ne(Oe(Ai)()(Ka)()(Qa)),$A=Ne(Oe(ki)()(hi)()(Ka)),OU=wr(),wU=St(St(Or)(Zv)()()()({reflectSymbol:function(){return"fftSize"}})),MU={reflectSymbol:function(){return"cb"}},IU=$r()(),PU=at(),NU=Lu(vs),RU=gt({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})),LU=Ns(cu),BU=ot(it),WU=P_(jt)(nt),Sr=$t(jt),Er=C(nt),qU=kt(Jc),UU=Ju(Vu),hA=A(Ct),HU=Ua(Yt)(Ue),zU=vm(sm),CA=Rr(Iu),VU=bn(Cx)(Yt),xA=A(R),JU=Ne(Oe(yx)()(sb)()(hx)),GU=Ne(Oe(Ax)()(vb)()(sb)),jU=Ne(Oe(kx)()(mb)()(vb)),XU=w_(fe),QU=Nn(Yt),KU=mn(fe),YU=bb(ta),Mr=kt(Cr),ZU="background-color: rgb(150,30,10);",t4="background-color: rgb(130,60,10);",r4="background-color: rgb(80,90,10);",e4="background-color: rgb(10,130,10);",n4="background-color: rgb(10,100,0);",a4=hE(function(t){return TA(ZU)(SA(t4)(EA(r4)(FA(e4)($A(n4)(Au)))))}),u4=function(t){var r=rm(tm(ar(OU(wU(t)()()()(MU)))(IU)));return function(e){var n=ur(e);return function(a){return function(u){return r({cb:u,fftSize:Wv.value})([n(a)(PU)])}}}},o4=u4(Yv)(Dr),i4=function(){return D.value}(),Ir="background-color: rgb(255,255,255,0.0);",f4=function(t){var r=A(t);return function(e){var n=db(e)();return function(a){return function(u){var i=db(u)();return function(o){return function(f){var p=kt(f);return function(l){return function(_){return function(m){return function(s){return function(c){return r(function(v){var K=n(i(v)(m))(s);return K?p(It.value)(n(i(a4)(m))(s)):p(It.value)(Ir)})(c)}}}}}}}}}}},N_=f4(Ct),Df=N_(sn)(Uo),c4=Df(sn)(ao)(Cr)(Uo)(ao),l4=Df(yu)(no)(Cr)(Uo)(no),p4=Df(bu)(eo)(Cr)(Uo)(eo),_4=Df(du)(ro)(Cr)(Uo)(ro),s4=Df(Du)(to)(Cr)(Uo)(to),v4=Df(ta)(Zu)(Cr)(Uo)(Zu),m4=Df(Ro)(Yu)(Cr)(Uo)(Yu),D4=Df(No)(Ku)(Cr)(Uo)(Ku),df=N_(yu)(qo),d4=df(sn)(ao)(Cr)(qo)(ao),b4=df(yu)(no)(Cr)(qo)(no),y4=df(bu)(eo)(Cr)(qo)(eo),A4=df(du)(ro)(Cr)(qo)(ro),k4=df(Du)(to)(Cr)(qo)(to),g4=df(ta)(Zu)(Cr)(qo)(Zu),h4=df(Ro)(Yu)(Cr)(qo)(Yu),C4=df(No)(Ku)(Cr)(qo)(Ku),bf=N_(bu)(Wo),x4=bf(sn)(ao)(Cr)(Wo)(ao),T4=bf(yu)(no)(Cr)(Wo)(no),S4=bf(bu)(eo)(Cr)(Wo)(eo),E4=bf(du)(ro)(Cr)(Wo)(ro),F4=bf(Du)(to)(Cr)(Wo)(to),$4=bf(ta)(Zu)(Cr)(Wo)(Zu),O4=bf(Ro)(Yu)(Cr)(Wo)(Yu),w4=bf(No)(Ku)(Cr)(Wo)(Ku),yf=N_(du)(Bo),M4=yf(sn)(ao)(Cr)(Bo)(ao),I4=yf(yu)(no)(Cr)(Bo)(no),P4=yf(bu)(eo)(Cr)(Bo)(eo),N4=yf(du)(ro)(Cr)(Bo)(ro),R4=yf(Du)(to)(Cr)(Bo)(to),L4=yf(ta)(Zu)(Cr)(Bo)(Zu),B4=yf(Ro)(Yu)(Cr)(Bo)(Yu),W4=yf(No)(Ku)(Cr)(Bo)(Ku),Af=N_(Du)(Lo),q4=Af(sn)(ao)(Cr)(Lo)(ao),U4=Af(yu)(no)(Cr)(Lo)(no),H4=Af(bu)(eo)(Cr)(Lo)(eo),z4=Af(du)(ro)(Cr)(Lo)(ro),V4=Af(Du)(to)(Cr)(Lo)(to),J4=Af(ta)(Zu)(Cr)(Lo)(Zu),G4=Af(Ro)(Yu)(Cr)(Lo)(Yu),j4=Af(No)(Ku)(Cr)(Lo)(Ku),X4=function(){return 15/40}(),Q4=function(){return 10/40}(),K4=function(){return 7/40}(),Y4=function(){return 3/40}(),Z4=function(){return 1/40}(),CE=function(t){return function(r){return function(e){return RU(i4)({analyser:$(He(function(n){return function(a){var u=LU(BU)(a),i=WU(e)(function(f){return f.right}(u)),o=function(f){return f.left}(u);return Ur([on(Sr(Er(qU(It.value)("cursor: pointer;")))(lm(t)(function(f){return n(zt.create(f))})(function(f){return lt(f)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(f){return function(p){return function(){var _=Nr(F.value)(),m=vf(f)(),s=UU(We(0))(),c=_c([o4(p)(function(K){return function(){return te(new h(K))(_)(),te(F.value)(_)}})])(Dc(s)),v=Pe(Sr(hA(zt.create)(c))(hA(Ht.create)(mf)))(function(K){if(K instanceof zt)return K.value0(m);if(K instanceof Ht)return function(){var xt=zr(_)();return HU(xt)(function(xr){return function(){var Z=$_(xr)(),Xt=zU(Z)(),At=Nr(0)(),jr=Nr(0)(),Jr=Nr(0)(),fn=Nr(0)(),In=Nr(0)(),nu=Nr(0)(),xe=Nr(0)(),pe=Nr(0)(),Pa=Nr(0)(),Ko=Nr(0)(),kc=function(hn){if(hn<32)return At;if(hn<64)return jr;if(hn<96)return Jr;if(hn<128)return fn;if(hn<168)return In;if(hn<160)return nu;if(hn<224)return xe;if(Yr)return pe;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 146, column 45 - line 154, column 63): "+[hn.constructor.name])};np(Xt)(function(hn){var Pf=mm(hn);return function(){var Yo=zr(Ko)();return ni(CA(Pf))(Pa)(),ni(CA(Pf))(kc(Yo))(),ni(CA(1))(Ko)()}})();var ca=VU(function(hn){return function(){var cs=xA(Vr)(zr(hn))(),Yo=xA(NU(cs))(xA(Vr)(zr(Pa)))();return TA(Yo>X4)(SA(Yo>Q4)(EA(Yo>K4)(FA(Yo>Y4)($A(Yo>Z4)(Au)))))}})(JU(At)(GU(jr)(jU(Jr)(TA(fn)(SA(In)(EA(nu)(FA(xe)($A(pe)(Au)))))))))();return n(new Ht(ca))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 127, column 57 - line 165, column 57): "+[K.constructor.name])})();return function(){return v(),function(){var xt=XU(f)();return QU(xt!=="closed")(KU(f))()}(),n(new Ht(hE(g(YU(g(!1))))))()}}}})(e)(i)))([Qe(hA(function(f){if(f instanceof ru)return"Turn on";if(f instanceof jo)return"Loading...";if(f instanceof Xo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 178, column 31 - line 181, column 56): "+[f.constructor.name])})(i))]),yr(Er(Mr(It.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([yr(Sr(Er(Mr(It.value)(Ir)))(c4(wa)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(l4(Oa)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(p4($a)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(_4(Fa)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(s4(Ea)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(v4(Sl)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(m4(Tl)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(D4(xl)(wa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(d4(wa)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(b4(Oa)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(y4($a)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(A4(Fa)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(k4(Ea)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(g4(Sl)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(h4(Tl)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(C4(xl)(Oa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(x4(wa)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(T4(Oa)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(S4($a)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(E4(Fa)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(F4(Ea)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))($4(Sl)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(O4(Tl)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(w4(xl)($a)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(M4(wa)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(I4(Oa)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(P4($a)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(N4(Fa)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(R4(Ea)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(L4(Sl)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(B4(Tl)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(W4(xl)(Fa)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(q4(wa)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(U4(Oa)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(H4($a)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(z4(Fa)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(V4(Ea)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(J4(Sl)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(G4(Tl)(Ea)(o)))([]),yr(Sr(Er(Mr(It.value)(Ir)))(j4(xl)(Ea)(o)))([])])])}}))})}}};var rH=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})),eH=ur(Dr),nH=at(),aH=Nt(dt),R_=Vo(io(ar(wr()(St(St(Or)(zo)()()()({reflectSymbol:function(){return"q"}}))(uo)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),uH=function(){return D.value}(),xE=function(t){return function(r){return function(e){return rH(D.value)(uH)({bandpass:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([ea(eH(a)(nH))(function(u){return function(i){return aH(.8)([R_({frequency:400,q:1})([u]),R_({frequency:880,q:5})([u]),R_({frequency:1200,q:10})([u]),R_({frequency:2e3,q:20})([u]),R_({frequency:3e3,q:30})([u])])}})])}}))})}}};var iH=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})),fH=AS(iS(ar(wr()(Or))($r()()))),cH=ur(Dr),lH=at(),pH=function(){return D.value}(),TE=function(t){return function(r){return function(e){return iH(pH)({compression:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([fH({})([cH(a)(lH)])])}}))})}}};var kf=ui(),gf=bi();var sH=gf({reflectSymbol:function(){return"playbackRate"}}),vH=gf({reflectSymbol:function(){return"onOff"}}),mH=gf({reflectSymbol:function(){return"offset"}}),DH=gf({reflectSymbol:function(){return"loopStart"}}),dH=gf({reflectSymbol:function(){return"loopEnd"}}),bH=gf({reflectSymbol:function(){return"gain"}}),yH=gf({reflectSymbol:function(){return"frequency"}});var AH=gf({reflectSymbol:function(){return"delayTime"}});var Ma=function(){return function(t){var r=sH(D.value),e=pc(t);return function(n){return kf(r(e(n)))}}},dc=function(){return function(t){var r=vH(D.value),e=uT(t);return function(n){return kf(r(e(n)))}}},SE=function(){return function(t){var r=mH(D.value),e=pc(t);return function(n){return kf(r(e(n)))}}},EE=function(){var t=DH(D.value);return function(r){return kf(t(r))}},FE=function(){var t=dH(D.value);return function(r){return kf(t(r))}},da=function(){return function(t){var r=bH(D.value),e=pc(t);return function(n){return kf(r(e(n)))}}},hf=function(){return function(t){var r=yH(D.value),e=pc(t);return function(n){return kf(r(e(n)))}}};var OA=function(){return function(t){var r=AH(D.value),e=pc(t);return function(n){return kf(r(e(n)))}}};var wA=Q(),kH=gt({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(wA(wA(wA(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})),gH=C(ua),hH=Nt(dt),CH=nm(Jv),xH=$t(jt),TH=at(),SH=C(nt),EH=SE()(wn),FH=bo(ii),$H=ha(Ru),OH=function(){return D.value}(),$E=function(t){return function(r){return function(e){return kH(OH)({tf:$(ee("<|>")),txt:$(ee(`run2_
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
  ]`)),constant:$(bt(e)(t)(function(n){return gH(void 0)})(function(n){return function(a){return ut(n)([hH(.5)([CH(0)(xH(TH)(SH(EH({d:5,o:.1,p:FH(function(u){return g(function(){var i=$H(u)(3)===0;return i?1:0}())})(Ge(0)(1920))}))))])])}}))})}}};var MH=gt({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})),IH=Dt(GS),PH=A(Ci),NH=vS(gT),RH=ur(Dr),LH=at(),BH=function(){return D.value}(),OE=function(t){return function(r){return function(e){return MH(BH)({convolution:$(bt(e)(t)(function(n){return IH(PH(function(a){return function(u){return{loop:a,verb:u}}})(lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(lt(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return ut(n)([NH(a.verb)([RH(a.loop)(LH)])])}}))})}}};var qH=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})),UH=ra(hu),HH=at(),zH=Nt(dt),dm=sf(oo),VH=function(){return D.value}(),wE=function(t){return function(r){return function(e){return qH(D.value)(VH)({delay:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return ut(n)([ea(UH(a)(HH))(function(u){return function(i){return zH(.2)([dm(.03)([u]),dm(.1)([u]),dm(.3)([u]),dm(.7)([u])])}})])}}))})}}};var GH=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})),jH=Nt(dt),XH=ur(Dr),QH=at(),KH=function(){return D.value}(),ME=function(t){return function(r){return function(e){return GH(KH)({gain:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return ut(n)([jH(.1)([XH(a)(QH)])])}}))})}}};var ZH=gt({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})),tz=dl(ml),rz=ur(Dr),ez=at(),nz=function(){return D.value}(),IE=function(t){return function(r){return function(e){return ZH(nz)({highpass:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([tz(2e3)([rz(a)(ez)])])}}))})}}};var uz=gt({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})),oz=kS(oS(ar(wr()(St(St(Or)(IT)()()()({reflectSymbol:function(){return"gain"}}))(PT)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),iz=ur(Dr),fz=at(),cz=function(){return D.value}(),PE=function(t){return function(r){return function(e){return uz(cz)({highshelf:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([oz({frequency:2e3,gain:.4})([iz(a)(fz)])])}}))})}}};var pz=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})),_z=mS()()(kT(of)(of)),El=i0()(),sz=ur(Dr),vz=at(),mz=function(){return D.value}(),NE=function(t){return function(r){return function(e){return pz(mz)({iirFilterEx:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([_z(new G(El(20298e-8)(El(.0004059599)(El(20298e-8)(Ad))),El(1.0126964558)(El(-1.9991880801)(El(.9873035442)(Ad)))))([sz(a)(vz)])])}}))})}}};var dz=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})),LE=wr(),BE=St(St(Or)(Xv)()()()({reflectSymbol:function(){return"playbackRate"}})),WE={reflectSymbol:function(){return"buffer"}},qE=$r()(),RE=ur(vl(ar(LE(St(St(BE(jv)()()()({reflectSymbol:function(){return"loopStart"}}))(Gv)()()()({reflectSymbol:function(){return"loopEnd"}}))(pl)()()()(WE)))(qE))),MA=at(),bz=ur(vl(ar(LE(BE(pl)()()()(WE)))(qE))),yz=function(){return D.value}(),UE=function(t){return function(r){return function(e){return dz(D.value)(yz)({loopBuf:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return ut(n)([RE({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(MA),RE({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(MA),bz({buffer:a,playbackRate:1.7})(MA)])}}))})}}};var kz=gt({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})),gz=bl(Qv),hz=ur(Dr),Cz=at(),xz=function(){return D.value}(),HE=function(t){return function(r){return function(e){return kz(xz)({lowpass:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([gz(215)([hz(a)(Cz)])])}}))})}}};var Sz=gt({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})),Ez=hS(uS(ar(wr()(St(St(Or)(OT)()()()({reflectSymbol:function(){return"gain"}}))(wT)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),Fz=ur(Dr),$z=at(),Oz=function(){return D.value}(),zE=function(t){return function(r){return function(e){return Sz(Oz)({lowshelf:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([Ez({frequency:91,gain:.4})([Fz(a)($z)])])}}))})}}};var Mz=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})),IA=Nt(dt),Iz=y_(v_),Pz=sf(oo),Nz=bS(_f),Rz=function(){return D.value}(),VE=function(t){return function(r){return function(e){return Mz(D.value)(Rz)({microphone:$(bt(e)(t)(function(n){return fm(!0)(!1)})(function(n){return function(a){return ut(n)([function(){if(a.microphone instanceof h)return Sa(function(u){return IA(1)([Iz(a.microphone.value0),Pz(.1)([IA(.2)([u])])])});if(a.microphone instanceof F)return IA(.02)([Nz(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[a.microphone.constructor.name])}()])}}))})}}};var Bz=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})),L_=CS(aS(ar(wr()(St(St(Or)(FT)()()()({reflectSymbol:function(){return"q"}}))($T)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),B_=C(Mu),Wz=ur(Dr),qz=at(),Uz=function(){return D.value}(),JE=function(t){return function(r){return function(e){return Bz(Uz)({notch:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([L_({frequency:400,q:1})(B_(L_({frequency:880,q:5})(B_(L_({frequency:1200,q:10})(B_(L_({frequency:2e3,q:20})(B_(L_({frequency:3e3,q:30})(B_(Wz(a)(qz)))))))))))])}}))})}}};var zz=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})),W_=TS(nS(ar(wr()(St(St(St(Or)(TT)()()()({reflectSymbol:function(){return"q"}}))(ST)()()()({reflectSymbol:function(){return"gain"}}))(ET)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),q_=C(Mu),Vz=ur(Dr),Jz=at(),Gz=function(){return D.value}(),GE=function(t){return function(r){return function(e){return zz(Gz)({peaking:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([W_({frequency:400,q:1,gain:-20})(q_(W_({frequency:880,q:5,gain:20})(q_(W_({frequency:1200,q:10,gain:-20})(q_(W_({frequency:2e3,q:20,gain:20})(q_(W_({frequency:3e3,q:30,gain:-20})(q_(Vz(a)(Jz)))))))))))])}}))})}}};var Xz=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),Qz=C(ua),Kz=Nt(dt),Yz=Dl(sl(ar(wr()(St(St(Or)(_l(fl(Qu)))()()()({reflectSymbol:function(){return"spec"}}))(ll)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),jE=Ne(Oe(Qu)()(Xa)()(gi)),XE=Ne(Oe(yi)()(Qa)()(Xa)),QE=Ne(Oe(Ai)()(Ka)()(Qa)),KE=Ne(Oe(ki)()(hi)()(Ka)),Zz=at(),t5=function(){return D.value}(),YE=function(t){return function(r){return function(e){return Xz(t5)({periodic:$(bt(e)(t)(function(n){return Qz(void 0)})(function(n){return function(a){return ut(n)([Kz(.2)([Yz({frequency:140,spec:new G(jE(.1)(XE(.2)(QE(.3)(KE(.4)(Au)))),jE(.4)(XE(.3)(QE(.2)(KE(.1)(Au)))))})(Zz)])])}}))})}}};var e5=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})),n5=ra(D_(ar(wr()(St(St(St(Or)(CT)()()()({reflectSymbol:function(){return"duration"}}))(hT)()()()({reflectSymbol:function(){return"bufferOffset"}}))(m_)()()()({reflectSymbol:function(){return"buffer"}})))($r()()))),a5=at(),u5=function(){return D.value}(),ZE=function(t){return function(r){return function(e){return e5(u5)({playBuf:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return ut(n)([n5({buffer:a,duration:3,bufferOffset:4.2})(a5)])}}))})}}};var PA=function(){function t(){}return t.value=new t,t}();var tF={attr:function(t){return function(r){return d({key:"controls",value:P(r)})}}};var NA=function(){function t(){}return t.value=new t,t}();var rF={attr:function(t){return function(r){return d({key:"src",value:P(r)})}}};var eF=M(),RA=function(t){return function(r){return new S(V("audio")(t)(eF(I(eF(U(z)(r))))))}};var bc=function(){function t(){this.head=null,this.last=null,this.size=0}function r(l,_){this.queue=l,this.value=_,this.next=null,this.prev=null}function e(l){this.draining=!1,this.error=null,this.value=l,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(l){try{l()}catch(_){setTimeout(function(){throw _},0)}}function u(l,_){var m=new r(l,_);switch(l.size){case 0:l.head=m;break;case 1:m.prev=l.head,l.head.next=m,l.last=m;break;default:m.prev=l.last,l.last.next=m,l.last=m}return l.size++,m}function i(l){var _;switch(l.size){case 0:return null;case 1:_=l.head,l.head=null;break;case 2:_=l.last,l.head.next=null,l.last=null;break;default:_=l.last,l.last=_.prev,l.last.next=null}return _.prev=null,_.queue=null,l.size--,_.value}function o(l){var _;switch(l.size){case 0:return null;case 1:_=l.head,l.head=null;break;case 2:_=l.head,l.last.prev=null,l.head=l.last,l.last=null;break;default:_=l.head,l.head=_.next,l.head.prev=null}return _.next=null,_.queue=null,l.size--,_.value}function f(l){if(l.queue!==null){if(l.queue.last===l){i(l.queue);return}if(l.queue.head===l){o(l.queue);return}l.prev&&(l.prev.next=l.next),l.next&&(l.next.prev=l.prev),l.queue.size--,l.queue=null,l.value=null,l.next=null,l.prev=null}}function p(l,_){if(!_.draining){var m=_.puts,s=_.takes,c=_.reads,v,K,pt,xt,xr;for(_.draining=!0;;){if(v=null,K=null,pt=null,xt=_.value,xr=c.size,_.error!==null){for(xt=l.left(_.error);v=o(m);)a(v.cb(xt));for(;K=o(c);)a(K(xt));for(;pt=o(s);)a(pt(xt));break}if(xt===n&&(v=o(m))&&(_.value=xt=v.value),xt!==n){for(pt=o(s);xr--&&(K=o(c));)a(K(l.right(xt)));pt!==null&&(_.value=n,a(pt(l.right(xt))))}if(v!==null&&a(v.cb(l.right(void 0))),_.value===n&&m.size===0||_.value!==n&&s.size===0)break}_.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=i,e.takeHead=o,e.deleteCell=f,e.drainVar=p,e}();function LA(){return new bc(bc.EMPTY)}function nF(t,r,e){return function(){return e.value===bc.EMPTY&&e.error===null?(e.value=r,bc.drainVar(t,e),!0):!1}}function aF(t,r){return function(){var e=r.value;return e===bc.EMPTY?t.nothing:(r.value=bc.EMPTY,bc.drainVar(t,r),t.just(e))}}var p5=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),_5=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),s5=function(){function t(){}return t.value=new t,t}();var uF=function(){return{left:Ht.create,right:zt.create,nothing:F.value,just:h.create,killed:p5.create,filled:_5.create,empty:s5.value}}();var oF=function(t){return function(r){return nF(uF,t,r)}};var iF=function(t){return aF(uF,t)};var m5=gt({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})),fF=Ns(cu),cF=ot(it),D5=P_(jt)(nt),Fl=$t(jt),U_=C(nt),d5=kt(Jc),Cf=A(Ct),b5=kt(Se),H_=C(Yt),BA=or(vr),y5=Ua(Yt)(Ue),lF=iv(qC),A5=yt(Kn),k5=yt(aa),g5=A(Ci),h5=Ee(Ze),C5=Mn(fe),x5=Ju(Vu),T5=Wr(R),S5=dn(Yt)(Ue),E5=w_(fe),F5=Nn(Yt),$5=mn(fe),pF=Dt(yn),O5=kt(tF),_F=kt(z0),w5=kt(rF),M5=function(t){var r=Zb(t);return function(e){var n=y_(e);return function(a){return function(u){return r(u)(n(a))}}}},I5=M5(Nb)(v_),P5=function(){return D.value}(),sF=function(t){return function(r){return function(e){return m5(P5)({recorder:$(He(function(n){return function(a){var u=fF(cF)(a),i=fF(cF)(function(l){return l.left}(u)),o=function(l){return l.right}(i),f=D5(e)(function(l){return l.right}(u)),p=function(l){return l.left}(i);return Ur([on(Fl(U_(d5(It.value)("cursor: pointer;")))(Cf(function(l){return b5(ie.value)(Kr(g(function(){if(l.e instanceof jo)return H_(void 0);if(l.e instanceof Xo)return BA(BA(BA(l.e.value0)(t(H_(void 0))))(y5(l.rec)(function(_){return lF(om(_))})))(n(zt.create(ru.value)));if(l.e instanceof ru)return function(){l.cncl();var m=LA();n(new zt(jo.value))();var s=co(k5(g5(function(c){return c.microphone})(fm(!0)(!1)))(function(c){return h5(function(){var K=Ft(H_(H_(void 0)))(function(pt){return function(){var xr=C5(),ce=vf(xr)(),Z=x5(We(0))(),Xt=_c([I5(pt)(function(jr){return function(){return n(new Ht(new zt(jr)))(),T5(oF(jr)(m))(),fE("audio/ogg; codecs=opus")(function(fn){return n(Ht.create(Ht.create(fn)))})(jr)()}})])(Dc(Z)),At=Pe(Xt)(function(jr){return jr(ce)})();return function(){At(),A5(iF(m))(S5(function(fn){return lF(om(fn))}))();var Jr=E5(xr)();return F5(Jr!=="closed")($5(xr))()}}})(c)();return n(new zt(new Xo(K)))(),K})}))();return t(function(){return n(zt.create(ru.value))(),Go(xi(s))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 67, column 45 - line 111, column 50): "+[l.e.constructor.name])}())))})(pF(Cf(gc)(pF(Cf(function(l){return function(_){return function(m){return{e:l,cncl:_,rec:m}}}})(f))(Fl(U_(H_(void 0)))(Cf(function(l){return l.value0})(e)))))(Fl(U_(F.value))(Cf(h.create)(o))))))([Qe(Cf(function(l){if(l instanceof ru)return"Turn on";if(l instanceof jo)return"Loading...";if(l instanceof Xo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 123, column 29 - line 126, column 54): "+[l.constructor.name])})(f))]),Ur([RA(Fl(U_(O5(PA.value)("true")))(Fl(U_(_F(It.value)("display:none;")))(Fl(Cf(function(l){return w5(NA.value)(l)})(p))(Cf(g(_F(It.value)("display:block;")))(p)))))([])])])}}))})}}};var R5=gt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),L5=C(ua),B5=Nt(dt),W5=dS(AT),q5=at(),U5=function(){return D.value}(),vF=function(t){return function(r){return function(e){return R5(U5)({periodic:$(bt(e)(t)(function(n){return L5(void 0)})(function(n){return function(a){return ut(n)([B5(.2)([W5(448)(q5)])])}}))})}}};var z5=gt({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),V5=C(ua),J5=Nt(dt),G5=sc(_f),j5=at(),X5=function(){return D.value}(),mF=function(t){return function(r){return function(e){return z5(X5)({periodic:$(bt(e)(t)(function(n){return V5(void 0)})(function(n){return function(a){return ut(n)([J5(.2)([G5(448)(j5)])])}}))})}}};var K5=gt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),Y5=C(ua),Z5=Nt(dt),tV=A_(cl),rV=at(),eV=function(){return D.value}(),DF=function(t){return function(r){return function(e){return K5(eV)({periodic:$(bt(e)(t)(function(n){return Y5(void 0)})(function(n){return function(a){return ut(n)([Z5(.2)([tV(448)(rV)])])}}))})}}};var aV=gt({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})),uV=xS(yT),oV=ur(Dr),iV=at(),fV=function(){return D.value}(),dF=function(t){return function(r){return function(e){return aV(fV)({pan:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return ut(n)([uV(1)([oV(a)(iV)])])}}))})}}};var lV=function(){return D.value}(),bF=gt({reflectType:function(){return`<ul>
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
`}})()()(J)(lV)({});var _V=gt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(Q()(J)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})),sV=C(ua),vV=Nt(dt),mV=em(Vv),DV=at(),dV=function(){return D.value}(),yF=function(t){return function(r){return function(e){return _V(dV)({periodic:$(bt(e)(t)(function(n){return sV(void 0)})(function(n){return function(a){return ut(n)([vV(.2)([mV(448)(DV)])])}}))})}}};var AF=Q(),yV=gt({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(AF(AF(J)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),AV=A(Ar),kV=_D(Pu)(Bf),gV=SS(eS),hV=ur(Dr),CV=at(),xV=function(){return D.value}(),kF=function(t){return function(r){return function(e){return yV(xV)({code:$(ee(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(i){var o=tf/180;return AV(function(f){var p=Vr(f)*2/Vr(44100)-1;return(3+i)*p*20*o/(tf+i*kV(p))})(Ge(0)(44099))};return ut(n)([gV(lA(u(400)))([hV(a)(CV)])])}}))})}}};var SV=or(vr),ke=Q(),EV=gt({reflectType:function(){return`<div>
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
</div>`}})()()(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(Ke()(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(ke(J)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})),FV=Nt(dt),$V=ur(Dr),OV=at(),wV=oa(jt)(nt),MV=function(){return D.value}(),gF=function(t){return function(r){return function(e){return function(n){var a=SV(r(mc.value))(tn),u=ia(t)(e);return EV(MV)({drumroll:$(pm("\u{1F941}")(n)(u)(function(i){return lt(i)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(i){return function(o){return ut(i)([FV(1)([$V(o)(OV)])])}})),toc:$(bF),allpass:$(dE(u)(r)(n)),analyser:$(CE(u)(r)(n)),bandpass:$(xE(u)(r)(n)),constant:$($E(u)(r)(n)),compression:$(TE(u)(r)(n)),convolution:$(OE(u)(r)(n)),delay:$(wE(u)(r)(n)),gain:$(ME(u)(r)(n)),highpass:$(IE(u)(r)(n)),highshelf:$(PE(u)(r)(n)),iirFilter:$(NE(u)(r)(n)),loopBuf:$(UE(u)(r)(n)),lowshelf:$(zE(u)(r)(n)),lowpass:$(HE(u)(r)(n)),notch:$(JE(u)(r)(n)),playBuf:$(ZE(u)(r)(n)),peaking:$(GE(u)(r)(n)),microphone:$(VE(u)(r)(n)),pan:$(dF(u)(r)(n)),periodicOsc:$(YE(u)(r)(n)),recorder:$(sF(u)(r)(n)),sawtoothOsc:$(vF(u)(r)(n)),sinOsc:$(mF(u)(r)(n)),squareOsc:$(DF(u)(r)(n)),triangleOsc:$(yF(u)(r)(n)),waveShaper:$(kF(u)(r)(n)),next:wV(n)(a)})}}}};var WA=function(){function t(){}return t.value=new t,t}(),hF={attr:function(t){return function(r){return d({key:"checked",value:P(r)})}}};var lo=function(){function t(){}return t.value=new t,t}();var Qo={attr:function(t){return function(r){return d({key:"type",value:P(r)})}}};var CF=M(),po=function(t){return function(r){return new S(V("input")(t)(CF(I(CF(U(z)(r))))))}};var RV=$u(Zo);var LV=function(t){return t},bm=function(t){var r=_i(t),e=t.Alternative0(),n=$t(e.Plus1().Alt0()),a=C(e.Applicative0());return function(u){return function(i){return r(n(a(u))(i))}}};var J_=function(t){return function(r){return t(r)}},$l=function(t){var r=A(t);return{map:function(e){return function(n){return function(a){return n(r(function(u){return function(i){return u(e(i))}})(a))}}}}},Si=function(t){var r=A($l(t)),e=A(t);return function(n){return function(a){return function(u){return J_(r(n)(a))(e(Rf)(u))}}}};var ym=function(t){return Si(t)(g)};var Ia=LV;var xF=function(t){var r=Co(t),e=t.Alternative0(),n=$t(e.Plus1().Alt0()),a=C(e.Applicative0()),u=A(t.Filterable1().Functor1());return function(i){return function(o){return Ia(function(f){return r(n(a(J_(i)(f)))(u(function(p){return J_(p)(f)})(o)))})}}},TF=function(t){var r=A(t),e=$l(t);return{apply:function(n){return function(a){return function(u){return a(n(r(RV)(u)))}}},Functor0:function(){return e}}};var SF=yt(Kn),EF=Nn(Yt),BV=Ri(Pu),FF=dn(Yt)(Ue),$F=or(vr);var Ol=function(t){return function(r){return Yn(function(e){return Pe(r)(function(n){return function(){var u=O_(t)();return e({acTime:u,value:n})()}})})}};var OF=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(i){return function(o){return function(f){return function(p){return function(){var _=zr(i)();return EF(_)(function(){var s=O_(t)(),c=Bs(dC(BV(u-s-.04)(.01)*1e3))(function(){var K=zr(i)();return EF(K)(function(){return te(u)(f)(),a(u)(),n(a)(u+p)(i)(o)(f)(p)()})()})();return te(new h(c))(o)()})()}}}}}}};return Yn(function(a){return function(){var i=Nr(!0)(),o=Nr(F.value)(),f=O_(t)(),p=Nr(f+r)();n(a)(r)(i)(o)(p)(r)();var l=Pe(e)(function(_){return function(){SF(zr(o))(FF(Sp))();var s=zr(p)();return n(a)(s+_)(i)(o)(p)(_)()}})();return $F($F(l)(te(!1)(i)))(SF(zr(o))(FF(Sp)))}})}}};var fa=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var i=(n-r)/(e-t),o=r-i*t;return i*a+o}}}}};var wF=Q(),WV=gt({reflectType:function(){return`<section>
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

</section>`}})()()(wF(wF(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),qV=vu()(pu({reflectSymbol:function(){return"cbx"}})()()()(re({reflectSymbol:function(){return"cbx0"}})()()(re({reflectSymbol:function(){return"cbx1"}})()()(re({reflectSymbol:function(){return"cbx2"}})()()(re({reflectSymbol:function(){return"cbx3"}})()()(An)()())()())()())()())(pu({reflectSymbol:function(){return"startStop"}})()()()(re({reflectSymbol:function(){return"start"}})()()(re({reflectSymbol:function(){return"stop"}})()()(An)()())()())(An)()())()()),MF=$t(jt),gm=C(nt),UV=bm($e),HV=fu($e),IF=Hn(Gt)(T),Tf=A(Ct),PF=kt(Se),hm=De(Ct),zV=Dt(yn),wl=j(Ct),VV=ot(it),NF=C(Yt),JV=Mn(fe),GV=Tu(fe),jV=Uc($e),XV=Rr(jn),QV=Si(Ct),RF=_i($e),LF=ym(Ct),KV=Da(dt),YV=da()(gu),ZV=Dl(sl(ar(wr()(St(St(Or)(_l(fl(Qu)))()()()({reflectSymbol:function(){return"spec"}}))(ll)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),BF=Ne(Oe(Qu)()(Xa)()(gi)),WF=Ne(Oe(yi)()(Qa)()(Xa)),qF=Ne(Oe(Ai)()(Ka)()(Qa)),UF=Ne(Oe(ki)()(hi)()(Ka)),qA=me(Gt)(T),t8=at(),r8=hf()(gu),G_=or(vr),e8=mn(fe),n8=kt(Cr),a8=A(Ar),u8=kt(Qo),o8=kt(hF),i8=uD(Ar),f8=function(){return D.value}(),HF=function(t){return function(r){return function(e){return function(n){return WV(f8)({txt:$(ee(`module Main where

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
import FRP.Event.Class (fold, mapAccum, sampleOnRight)
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
  )`)),empl:$(qV(D.value)(function(a){return function(u){var i=MF(gm(void 0))(u.startStop.start),o=function(m){return UV(!1)(HV(function(s){return function(c){return!s}})(!1)(m))},f=o(u.cbx.cbx3),p=o(u.cbx.cbx2),l=o(u.cbx.cbx1),_=o(u.cbx.cbx0);return Ur([on(IF(Tf(function(){var m=PF(ie.value);return function(s){return m(Kr(g(s)))}}()))([hm(zV(wl(i)(VV))(MF(gm(NF(void 0)))(Tf(function(m){return m.value0})(n))))(function(m){return function(){m();var c=JV(),v=GV(c)(),K=function(xr){return function(ce){return function(Z){return jV(function(Xt){return function(At){var jr=Xt.value1+(At.value1-Xt.value0)*function(){return At.value0?xr:1}();return new G(new G(At.value1,jr),jr)}})(new G(0,0))(QV(G.create)(ce)(Z))}}},pt=Cl(c)(Gu(Tf(function(){var xr=XV(.04);return function(ce){return xr(function(Z){return Z.acTime}(ce))}}())(Ol(c)(mf)))(function(xr){var ce=function(Jr){return function(fn){return RF(xr)(Tf(gc)(RF(fn)(Tf(function(In){return function(nu){return function(xe){return{f:In,a:nu,t:xe}}}})(Jr))))}},Z=Tf(function(Jr){return Jr?4:1})(LF(f)(xr)),Xt=K(4)(p)(xr),At=Tf(function(Jr){return Jr?4:1})(LF(l)(xr)),jr=K(8)(_)(xr);return[KV(0)(hm(ce(jr)(At))(function(Jr){return YV({n:fa(1)(.01)(4)(.15)(Jr.a)*ev(tf*Jr.f)+.15,o:Jr.t,t:Ho})}))([ZV({frequency:325.6,spec:new G(BF(.3)(WF(-.1)(qF(.7)(UF(-.4)(Au)))),BF(.6)(WF(.3)(qF(.2)(UF(0)(Au)))))})(qA([t8,hm(ce(Xt)(Z))(function(Jr){return r8({n:325.6+fa(1)(3)(4)(15.5)(Jr.a)*ev(tf*Jr.f),o:Jr.t,t:Ho})})]))])]}))(),xt=G_(G_(pt)(v))(e8(c));return t(G_(xt)(a.startStop.start(void 0)))(),a.startStop.stop(xt)()}}),hm(u.startStop.stop)(function(m){return G_(m)(G_(t(NF(void 0)))(a.startStop.start(void 0)))})]))([Qe(qA([wl(i)("Turn on"),wl(u.startStop.stop)("Turn off")]))]),yr(IF(Tf(n8(It.value)))([wl(u.startStop.stop)("display:block;"),wl(i)("display:none;")]))(a8(function(m){return po(qA([gm(u8(lo.value)("checkbox")),gm(PF(ie.value)(Kr(g(m(void 0))))),wl(i)(o8(WA.value)("false"))]))([])})(i8([function(m){return m.cbx0},function(m){return m.cbx1},function(m){return m.cbx2},function(m){return m.cbx3}])(a.cbx)))])}}))})}}}};var zF={recip:function(t){return 1/t},Ring0:function(){return Bf}};var VF=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function Ml(t){return function(){return function(r){return t(r)()}}}function Il(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function Pl(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function UA(t){return t.clientX}function HA(t){return t.clientY}function j_(t){return t.button}var X_=Tt("MouseEvent");var JF=A(R),Cm=dn(Yt)(Ue),Sf=ui();var m8=$h(Ie),D8=Ls(Ie);var GF=function(t){return function(r){return Yn(function(e){return Pe(r)(function(n){return function(){var u=zr(t.buttons)();return e({value:n,buttons:u})()}})})}};var jF=function(){var r=Nr(F.value)(),e=Nr(od)(),n=JF(fb)(di)(),a=Ml(function(f){return Cm(function(p){return te(new h({x:UA(p),y:HA(p)}))(r)})(X_(f))})(),u=Ml(function(f){return Cm(function(p){return zf(m8(j_(p)))(e)})(X_(f))})(),i=Ml(function(f){return Cm(function(p){return zf(D8(j_(p)))(e)})(X_(f))})();Il(Sf("mousemove"))(a)(!1)(n)(),Il(Sf("mousedown"))(u)(!1)(n)(),Il(Sf("mouseup"))(i)(!1)(n)();var o=function(){return Pl(Sf("mousemove"))(a)(!1)(n)(),Pl(Sf("mousedown"))(u)(!1)(n)(),Pl(Sf("mouseup"))(i)(!1)(n)()};return{position:r,buttons:e,dispose:o}},XF=Yn(function(t){return function(){var e=JF(fb)(di)(),n=Ml(function(a){return Cm(function(u){return t(j_(u))})(X_(a))})();return Il(Sf("mousedown"))(n)(!1)(e)(),Pl(Sf("mousedown"))(n)(!1)(e)}});var d8=A(Ct);var KF=function(t){return Ia(function(r){return d8(function(e){return e.value(e.buttons)})(GF(t)(r))})};var JA=function(t){return t};function xm(){return Date.now()}var h$=function(t){return Yn(function(r){return Pe(t)(function(e){return function(){var a=xm();return r({time:a,value:e})()}})})};var j8=A(Ct),X8=Ia(function(t){return j8(function(r){return r.value(r.time)})(h$(t))}),jA=A($l(Ct))(function(){var t=Al(HS);return function(r){return t(JA(r))}}())(X8);var YA=ot(it),K8=_i($e),Y8=qc($e),Z8=bm($e),$$=ym(Ct),t6=VF(vs)(zF),XA=A($l(Ct)),C$=he(),x$=Dt(TF(Ct)),r6=xF($e),Om=j(Ct),T$=Q(),e6=gt({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(T$(T$(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})),n6=vu()(re({reflectSymbol:function(){return"start"}})()()(re({reflectSymbol:function(){return"stop"}})()()(An)()())()()),Nl=$t(jt),S$=C(nt),a6=Hn(Gt)(T),Su=A(Ct),u6=kt(Se),E$=De(Ct),o6=Dt(yn),F$=C(Yt),i6=Mn(fe),f6=Tu(fe),QA=A(o_),c6=uc(GC),Tm=yt(dv),l6=C(i_),p6=Ne(Oe(Qu)()(Xa)()(gi)),_6=Ne(Oe(yi)()(Qa)()(Xa)),s6=Ne(Oe(Ai)()(Ka)()(Qa)),v6=Ne(Oe(ki)()(hi)()(Ka)),Sm=Dt(bv),Q_=Da(dt),K_=da()(gu),Ei=ul(ff),Y_=Ri(Pu),ZA=wr(),tk=St(Or),O$={reflectSymbol:function(){return"q"}},rk={reflectSymbol:function(){return"frequency"}},ek=$r()(),m6=bl(Xb(ar(ZA(St(tk(MT)()()()(O$))(Rb)()()()(rk)))(ek))),D6=yS(cl),KA=Vo(io(ar(ZA(St(tk(zo)()()()(O$))(uo)()()()(rk)))(ek))),Em=Dl(sl(ar(ZA(St(tk(_l(fl(Qu)))()()()({reflectSymbol:function(){return"spec"}}))(ll)()()()(rk)))(ek))),Fm=at(),$m=hf()(gu),Z_=or(vr),d6=mn(fe),b6=me(Gt)(T),y6=function(t){var r=function(i){var o=i.Filterable1().Functor1(),f=j(o),p=Us(i),l=Si(o),_=fu(i),m=_i(i);return function(s){var c=s.DivisionRing1().Ring0(),v=c.Semiring0(),K=Rr(v),pt=Xn(v),xt=Lu(s.EuclideanRing0()),xr=ln(v),ce=Ra(c);return function(Z){var Xt=Rr(Z);return function(At){return function(jr){return function(Jr){return function(fn){var In=K(pt)(pt),nu=function(xe){return function(pe){if(xe.last instanceof F)return pe;if(xe.last instanceof h)return Xt(pe)(At(function(Pa){return xt(xr(Pa(Xt(xe.last.value0.value1)(xe.now.value1)))(ce(xe.now.value0)(xe.last.value0.value0)))(In)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 106, column 5 - line 106, column 35): "+[xe.constructor.name,pe.constructor.name])}};return Ia(function(xe){var pe=J_(fn)(f(xe)(YA)),Pa=p(l(G.create)(Jr)(pe)),Ko=_(lr(nu))(jr)(Pa);return m(Ko)(xe)})}}}}}}},e=function(i){var o=r(i);return function(f){return o(f)(f.DivisionRing1().Ring0().Semiring0())(function(p){return p(YA)})}},n=e($e)(t6),a=function(i){return function(o){return Ia(function(f){return K8(Y8(function(p){var l=o(Z8(i)(p));return $$(l)(f)}))(f)})}},u=function(i){return function(o){return function(f){if(Fh(i))return-8*(o-1)-f*2;if(Yr)return 2*(4-o);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[i.constructor.name,o.constructor.name,f.constructor.name])}}};return a(2)(function(i){return n(2)(XA(C$)(jA))(function(){var o=a(10)(function(f){return n(10)(XA(C$)(jA))(x$(x$(XA(u)(KF(t)))(i))(f))});return r6(o)(Om(XF)(o))}())})},A6=function(){return D.value}(),w$=function(t){return function(r){return function(e){return function(n){return e6(A6)({txt:$(ee(`module Main

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
import FRP.Event.Class (class IsEvent, fix, fold, sampleOnRight, withLast)
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
  )`)),empl:$(n6(D.value)(function(a){return function(u){var i=Nl(S$(void 0))(u.start);return Ur([on(a6(Su(function(){var o=u6(ie.value);return function(f){return o(Kr(g(f)))}}()))([E$(o6(Om(i)(YA))(Nl(S$(F$(void 0)))(Su(function(o){return o.value0})(n))))(function(o){return function(){o();var p=i6(),l=f6(p)(),_=jF(),m=Gp(0)(1e4)(),s=function(Z){return{o:Z.value0+.04,n:Z.value1,t:Ho}},c=QA(function(Z){return Z-.5})(c6),v=Tm(c)(function(Z){return Tm(c)(function(Xt){return Tm(c)(function(At){return Tm(c)(function(jr){return l6(p6(Z)(_6(Xt)(s6(At)(v6(jr)(Au)))))})})})}),K=Sm(QA(G.create)(v))(v),pt=Sm(Sm(Sm(QA(function(Z){return function(Xt){return function(At){return function(jr){return{s0:Z,s1:Xt,s2:At,s3:jr}}}}})(K))(K))(K))(K),xt=uf(pt)({newSeed:rf(m),size:5}),xr=Cl(p)(Gu(Su(function(Z){return new G(Z.acTime,Z.value)})(Ol(p)($$(y6(_))(mf))))(function(Z){return[Q_(0)(Su(function(){var Xt=Ei(function(At){return Y_(-.4)(.5*(At-1))});return function(At){return K_(s(Xt(At)))}}())(Z))([m6({frequency:90.4,q:20})([D6(90.4)])]),Q_(0)(Su(function(){var Xt=Ei(function(At){return Y_(-.2)(.4*(At-3))});return function(At){return K_(s(Xt(At)))}}())(Z))([KA({frequency:90.4*4,q:20})([Em({frequency:90.4*3.02,spec:xt.s0})(Nl(Fm)(Su(function(){var Xt=Ei(function(At){return 273.00800000000004+14*(At-1)});return function(At){return $m(s(Xt(At)))}}())(Z)))])]),Q_(0)(Su(function(){var Xt=Ei(function(At){return Y_(-.1)(.2*(At-6))});return function(At){return K_(s(Xt(At)))}}())(Z))([KA({frequency:90.4*6,q:20})([Em({frequency:90.4*5.07,spec:xt.s1})(Nl(Fm)(Su(function(){var Xt=Ei(function(At){return 458.32800000000003+18*(At-1)});return function(At){return $m(s(Xt(At)))}}())(Z)))])]),Q_(0)(Su(function(){var Xt=Ei(function(At){return Y_(0)(.2*(At-3))});return function(At){return K_(s(Xt(At)))}}())(Z))([KA({frequency:90.4*8,q:20})([Em({frequency:90.4*7.13,spec:xt.s2})(Nl(Fm)(Su(function(){var Xt=Ei(function(At){return 644.552+32*(At-1)});return function(At){return $m(s(Xt(At)))}}())(Z)))])]),Q_(0)(Su(function(){var Xt=Ei(function(At){return Y_(0)(.1*(At-7))});return function(At){return K_(s(Xt(At)))}}())(Z))([Em({frequency:90.4*9.14,spec:xt.s3})(Nl(Fm)(Su(function(){var Xt=Ei(function(At){return 826.2560000000001+31*(At-1)});return function(At){return $m(s(Xt(At)))}}())(Z)))])]}))(),ce=Z_(Z_(xr)(l))(d6(p));return t(Z_(ce)(a.start(void 0)))(),a.stop(ce)()}}),E$(u.stop)(function(o){return Z_(o)(Z_(t(F$(void 0)))(a.start(void 0)))})]))([Qe(b6([Om(i)("Turn on"),Om(u.stop)("Turn off")]))])])}}))})}}}};var M$=Q(),g6=gt({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(M$(M$(Ke()(J)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})),h6=oa(jt)(nt),C6=or(vr),x6=function(){return D.value}(),I$=function(t){return function(r){return function(e){return function(n){var a=ia(t)(e);return g6(x6)({next:h6(n)(C6(r(F_.value))(tn)),fold:$(HF(a)(r)(e)(n)),fix:$(w$(a)(r)(e)(n))})}}}};var W$=Ta(Fn),uk=$t(jt),ok=C(nt),S6=da()(wn),E6=Da(dt),F6=sc(_f),P$=Q(),$6=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(P$(P$(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})),N$=De(Ct),O6=Dt(yn),R$=A(Ct),L$=C(Yt),w6=kt(Se),nk=or(vr),M6=Nt(dt),I6=so(_o),P6=A(Ar),N6=function(){function t(){}return t.value=new t,t}(),B$=function(){function t(){}return t.value=new t,t}(),ak=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),R6=`module Main where

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
`;var L6=function(){return D.value}(),B6=function(t){var r=C(t);return function(e){var n=dc(e)(lc);return function(a){return r(n({x:Pb,o:a}))}}},W6=B6(nt)(),q6=function(t){var r=C(t);return function(e){var n=dc(e)(lc);return function(a){return r(n({x:sT,o:a}))}}},U6=q6(nt)(),H6=W$(Vr)(function(t){var r=function(a){return uk(W6(a+.27*(t*Zi(1.005)(t))))(U6(a+3+.3*(t*Zi(1.005)(t))))},e=function(a){return ok(S6({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*Zi(1.005)(t)),d:.8}))},n=function(a){return function(u){return E6(0)(e(a))([F6(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),q$=function(t){return function(r){return function(e){return $6(D.value)(L6)({txt:$(ee(R6)),ex0:$(He(function(n){return W$(function(a){return uk(ok(N6.value))(a)})(function(a){return Ur([on(N$(O6(R$(G.create)(a))(uk(ok(L$(void 0)))(R$(function(u){return u.value0})(e))))(function(u){return w6(ie.value)(Kr(g(function(){return u.value0 instanceof ak?nk(nk(u.value0.value0)(n(B$.value)))(t(L$(void 0))):function(){u.value1();var o=_m([M6(1)(I6(P6(H6)(Ge(0)(100))))])();return t(nk(o)(n(B$.value)))(),n(new ak(o))()}}())))}))([Qe(N$(a)(function(u){return u instanceof ak?"Turn off":"Turn on"}))])])})}))})}}};var Fi=function(){function t(){}return t.value=new t,t}();var Ef={attr:function(t){return function(r){return d({key:"max",value:P(r)})}}};var $i=function(){function t(){}return t.value=new t,t}();var Ff={attr:function(t){return function(r){return d({key:"min",value:P(r)})}}};var Oi=function(){function t(){}return t.value=new t,t}();var $f={attr:function(t){return function(r){return d({key:"input",value:rt(r)})}}};var wi=function(){function t(){}return t.value=new t,t}(),Of={attr:function(t){return function(r){return d({key:"step",value:P(r)})}}};var Mi=function(){function t(){}return t.value=new t,t}();var wf={attr:function(t){return function(r){return d({key:"value",value:P(r)})}}};var Rm=function(t){var r=$t(t);return function(e){return function(n){return r(e)(n(void 0))}}};var V6=TC,J6=vt(V6),Rl={convert:function(t){return t}},Lm={convert:function(t){return Yp(t)}},H$=function(t){return t},ik=function(t){return t.convert},yc=function(t){var r=ik(t);return function(e){return function(n){return J6(Yp(e))(r(n(void 0)))}}};var G6=Hn(SC),Bm=function(t){var r=ik(t);return function(e){var n=G6(e);return function(a){return function(u){return n(a)(H$(r(u)))}}}};function V$(t){return t.target}var Ll=function(t){return ze(V$(t))};var lk=Q(),Q6=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(lk(lk(lk(J)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),K6=vu()(pu({reflectSymbol:function(){return"slider"}})()()()(re({reflectSymbol:function(){return"s0"}})()()(re({reflectSymbol:function(){return"s1"}})()()(re({reflectSymbol:function(){return"s2"}})()()(An)()())()())()())(pu({reflectSymbol:function(){return"startStop"}})()()()(re({reflectSymbol:function(){return"loading"}})()()(re({reflectSymbol:function(){return"start"}})()()(re({reflectSymbol:function(){return"stop"}})()()(An)()())()())()())(An)()())()()),pk=$t(jt),Wm=C(nt),Y6=ur(vl(ar(wr()(St(St(St(St(Or)(Xv)()()()({reflectSymbol:function(){return"playbackRate"}}))(jv)()()()({reflectSymbol:function(){return"loopStart"}}))(Gv)()()()({reflectSymbol:function(){return"loopEnd"}}))(pl)()()()({reflectSymbol:function(){return"buffer"}})))($r()()))),qm=Rm(jt),Z6=at(),Mf=A(Ct),tJ=Ma()(Hv),rJ=EE(),eJ=FE(),J$=Dt(yn),nJ=Rr(jn),aJ=vt(rn),uJ=A(Ar),G$=Bm(Rl)(T),ts=yc(Rl),oJ=kt(Qo),iJ=kt(Ff),fJ=kt(Ef),cJ=kt(Of),j$=yc(Lm),lJ=kt(wf),pJ=kt($f),_J=dn(Yt)(Ue),sJ=hc(Kn),vJ=xn(Un),mJ=kt(Se),X$=j(Ct),_k=C(Yt),Q$=De(Ct),Um=or(vr),DJ=ot(it),sk=yt(aa),dJ=Mn(Ze),bJ=Tu(Ze),yJ=Ee(Ze),AJ=mn(fe),kJ=`module Main where

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
`,gJ=function(){return D.value}(),hJ="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",K$=function(t){return function(r){return function(e){return Q6(D.value)(gJ)({wagtxt:$(ee(`run2_
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
          (add <$> (pure 0.0 <|> sl1))`)),txt:$(ee(kJ)),ex1:$(K6(D.value)(function(n){return function(a){var u=pk(a.startStop.start)(Wm(void 0)),i=function(o){return Y6({buffer:o,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(qm(Z6)(function(){return qm(Mf(function(){var f=fa(0)(.2)(100)(5);return function(p){return tJ(f(p))}}())(a.slider.s0))(function(){return qm(Mf(function(){var f=fa(0)(0)(100)(1.2);return function(p){return rJ(f(p))}}())(a.slider.s1))(function(){return Mf(function(){var f=fa(0)(.05)(100)(1);return function(p){return eJ(f(p))}}())(function(f){return J$(f)(a.slider.s2)}(Mf(nJ)(pk(Wm(0))(a.slider.s1))))})})}))};return Ur(aJ(uJ(function(o){return Ur([ee(o.l),po(G$(Wm)(ts(oJ(lo.value)("range"))(function(){return ts(iJ($i.value)("0"))(function(){return ts(fJ(Fi.value)("100"))(function(){return ts(cJ(wi.value)("1"))(function(){return j$(lJ(Mi.value)("50"))(function(){return pJ(Oi.value)(Kr(function(){var f=_J(sJ(ic)(o.f)),p=vJ(rl);return function(l){return f(p(Ll(l)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([on(G$(Mf(function(){var o=mJ(ie.value);return function(f){return o(Kr(g(f)))}}()))(ts(X$(a.startStop.loading)(_k(void 0)))(function(){return j$(Q$(a.startStop.stop)(function(o){return Um(o)(Um(t(_k(void 0)))(n.startStop.start(void 0)))}))(function(){return Q$(J$(X$(u)(DJ))(pk(Wm(_k(void 0)))(Mf(function(o){return o.value0})(e))))(function(o){return function(){o(),n.startStop.loading(void 0)();var p=co(sk(dJ)(function(l){return sk(bJ(l))(function(_){return sk(lt(l)(hJ))(function(m){return yJ(function(){var c=ut(l)([i(m)])(),v=Um(Um(c)(_))(AJ(l));return n.startStop.stop(v)(),v})})})}))();return t(function(){return n.startStop.start(void 0)(),Go(xi(p))()})(),void 0}})})})))([Qe(qm(Mf(g("Turn off"))(a.startStop.stop))(function(){return Mf(g("Turn on"))(u)}))])]))}}))})}}};var Y$=Q(),xJ=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Y$(Y$(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})),TJ=vu()(re({reflectSymbol:function(){return"slider"}})()()(pu({reflectSymbol:function(){return"startStop"}})()()()(re({reflectSymbol:function(){return"start"}})()()(re({reflectSymbol:function(){return"stop"}})()()(An)()())()())(An)()())()()),vk=$t(jt),mk=C(nt),Eu=A(Ct),SJ=Rr(jn),EJ=at(),FJ=hf()(Hv),$J=em(Vv),OJ=Nt(dt),Dk=Da(dt),dk=da()(wn),nO=wr(),aO=St(Or),uO={reflectSymbol:function(){return"q"}},oO={reflectSymbol:function(){return"frequency"}},iO=$r()(),Z$=Vo(io(ar(nO(St(aO(zo)()()()(uO))(uo)()()()(oO)))(iO))),wJ=dl(Qb(ar(nO(St(aO(NT)()()()(uO))(Lb)()()()(oO)))(iO))),MJ=Bm(Rl)(T),Hm=yc(Rl),IJ=kt(Qo),PJ=kt(Ff),NJ=kt(Ef),RJ=kt(Of),LJ=yc(Lm),BJ=kt(wf),WJ=kt($f),qJ=dn(Yt)(Ue),UJ=hc(Kn),HJ=xn(Un),zJ=Hn(Gt)(T),VJ=kt(Se),tO=De(Ct),JJ=Dt(yn),bk=j(Ct),GJ=ot(it),rO=C(Yt),jJ=Mn(fe),XJ=Si(Ct),rs=or(vr),eO=mn(fe),QJ=me(Gt)(T),KJ=`module Main where

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
  )`,YJ=Ia(function(t){return Yn(function(r){return Pe(t)(function(e){return function(){var a=Mo();return r(e(a))()}})})}),ZJ=function(){return D.value}(),t7=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(Yr)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 224, column 1 - line 224, column 23): "+[t.constructor.name])},fO=function(t){return function(r){return function(e){return xJ(D.value)(ZJ)({txt:$(ee(KJ)),ex2:$(TJ(D.value)(function(n){return function(a){var u=vk(a.startStop.start)(mk(void 0)),i=function(o){return Gu(o)(function(f){var p=Eu(function(){var v=SJ(.01);return function(K){return v(qe(K))}}())(f),l=Eu(_a)(f),_=vk(EJ)(Eu(function(v){return FJ(t7(v))})(l)),m=Eu(function(v){return qv(function(K){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:K}}(v))})(p),s=Eu(function(v){return qv(function(K){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:K}}(v))})(p),c=Eu(function(v){return qv(function(K){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:K}}(v))})(p);return[ea($J(0)(_))(function(v){return function(K){return OJ(2)([Dk(0)(Eu(dk)(c))([Z$({frequency:1e3,q:20})([v])]),Dk(0)(Eu(dk)(s))([Z$({frequency:2e3,q:20})([v])]),Dk(0)(Eu(dk)(m))([wJ({frequency:4e3,q:20})([v])])])}})]})};return Ur([Ur([ee("tempo"),po(MJ(mk)(Hm(IJ(lo.value)("range"))(function(){return Hm(PJ($i.value)("0"))(function(){return Hm(NJ(Fi.value)("100"))(function(){return Hm(RJ(wi.value)("1"))(function(){return LJ(BJ(Mi.value)("50"))(function(){return WJ(Oi.value)(Kr(function(){var o=qJ(UJ(ic)(n.slider)),f=HJ(rl);return function(p){return o(f(Ll(p)))}}()))})})})})})))([])]),on(zJ(Eu(function(){var o=VJ(ie.value);return function(f){return o(Kr(g(f)))}}()))([tO(JJ(bk(u)(GJ))(vk(mk(rO(void 0)))(Eu(function(o){return o.value0})(e))))(function(o){return function(){o();var p=jJ(),l=XJ(G.create)(YJ)(OF(p)(.91)(Eu(fa(0)(.42)(100)(1.4))(a.slider))),_=Cl(p)(i(l))(),m=rs(_)(eO(p));return t(rs(m)(n.startStop.start(void 0)))(),n.startStop.stop(rs(m)(eO(p)))()}}),tO(a.startStop.stop)(function(o){return rs(o)(rs(t(rO(void 0)))(n.startStop.start(void 0)))})]))([Qe(QJ([bk(u)("Turn on"),bk(a.startStop.stop)("Turn off")]))])])}}))})}}};var e7=function(){return D.value}(),cO=function(){return nr({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(J)(D.value)(e7)({})}();var a7=function(){return D.value}(),lO=function(){return nr({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(J)(D.value)(a7)({})}();var o7=function(){return D.value}(),pO=function(){return nr({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(J)(D.value)(o7)({})}();var f7=oa(jt)(nt),c7=or(vr),Bl=Q(),l7=nr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(Bl(Bl(Bl(Bl(Bl(Ke()(Bl(J)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}));var p7=function(){return D.value}(),_O=function(t){return function(r){return function(e){return function(n){var a=function(i){return f7(n)(c7(r(i))(tn))},u=ia(t)(e);return l7(D.value)(p7)({next:a(S_.value),primer:$(pO),inOcarina:$(lO),flavors:$(cO),ex0:$(q$(u)(r)(n)),ex1:$(K$(u)(r)(n)),ex2:$(fO(u)(r)(n))})}}}};var s7=nr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),v7=fo(Za),yk=Dt(h_),m7=A(gl),zm=Ya(Za),D7=Nt(dt),d7=C(nt),b7=dc()(lc),y7=Uv(),A7=Rr(jn),Vm=ra(hu),k7=function(){return D.value}(),sO=function(t){return function(r){return function(e){return s7(D.value)(k7)({ai0:$(bt(e)(t)(function(n){return v7(yk(yk(yk(m7(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(zm(lt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(zm(lt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(zm(lt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(zm(lt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return ut(n)([D7(1)(function(){var u=function(i){return d7(b7(y7(A7(i))(s_)))};return[Vm(a.tink0)(u(.1)),Vm(a.tink1)(u(.2)),Vm(a.tink2)(u(.9)),Vm(a.tink3)(u(1.8))]}())])}}))})}}};var h7=nr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),C7=fo(Za),Ak=Dt(h_),x7=A(gl),Jm=Ya(Za),T7=Nt(dt),S7=C(nt),E7=dc()(lc),F7=Uv(),$7=Rr(jn),O7=ha(Ru),w7=De(Ar),M7=ra(hu),I7=function(){return D.value}(),vO=function(t){return function(r){return function(e){return h7(D.value)(I7)({ai0:$(bt(e)(t)(function(n){return C7(Ak(Ak(Ak(x7(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Jm(lt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Jm(lt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Jm(lt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Jm(lt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return ut(n)([T7(1)(function(){var u=function(o){return S7(E7(F7($7(o))(s_)))},i=function(o){var f=O7(o)(4);return f===0?a.tink0:f===1?a.tink1:f===2?a.tink2:a.tink3};return w7(Ge(0)(100))(function(o){var f=Vr(o);return M7(i(o))(u(.3+.3*(f*Zi(1.005)(f))))})}())])}}))})}}};var N7=nr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),R7=ur(Dr),L7=at(),B7=Nt(dt),es=Vo(io(ar(wr()(St(St(Or)(zo)()()()({reflectSymbol:function(){return"q"}}))(uo)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),W7=function(){return D.value}(),mO=function(t){return function(r){return function(e){return N7(D.value)(W7)({ai0:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([ea(R7(a)(L7))(function(u){return function(i){return B7(.8)([es({frequency:400,q:1})([u]),es({frequency:880,q:5})([u]),es({frequency:1200,q:10})([u]),es({frequency:2e3,q:20})([u]),es({frequency:3e3,q:30})([u])])}})])}}))})}}};var U7=nr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),H7=ur(Dr),z7=at(),V7=Nt(dt),J7=De(Ar),G7=Ta(Fn),j7=Vo(io(ar(wr()(St(St(Or)(zo)()()()({reflectSymbol:function(){return"q"}}))(uo)()()()({reflectSymbol:function(){return"frequency"}})))($r()()))),X7=function(){return D.value}(),DO=function(t){return function(r){return function(e){return U7(D.value)(X7)({ai0:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([ea(H7(a)(z7))(function(u){return function(i){return V7(.8)(J7(Ge(0)(40))(G7(Vr)(function(o){return j7({frequency:200+o*150,q:30})([u])})))}})])}}))})}}};var K7=nr({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),dO=Nt(dt),Y7=ra(hu),Z7=at(),tG=sf(oo),rG=function(){return D.value}(),bO=function(t){return function(r){return function(e){return K7(D.value)(rG)({ai0:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return ut(n)([Sa(function(u){return dO(1)([Y7(a)(Z7),tG(.1)([dO(.6)([u])])])})])}}))})}}};var yO=Q(),nG=nr({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(yO(yO(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})),aG=ra(hu),uG=at(),AO=Nt(dt),kO=Da(dt),oG=function(){return D.value}(),iG=function(t){var r=C(t);return function(e){return r(da(e)(wn)({p:[1,1,0],o:0,d:10}))}},fG=iG(nt)(),cG=function(t){var r=C(t);return function(e){return r(da(e)(wn)({p:[1,1,0],o:0,d:8}))}},lG=cG(nt)(),pG=function(t){var r=sf(t);return function(e){var n=Nt(e);return function(a){var u=dl(a);return function(i){return function(o){return function(f){return function(p){return r(i)([n(o)([u(f)(p)])])}}}}}}},Wl=pG(oo)(dt)(ml),gO=function(t){return function(r){return function(e){return nG(D.value)(oG)({txt:$(ee(`dgh d g h i =
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
  ]`)),ai0:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return ut(n)([ea(aG(a)(uG))(function(u){return function(i){return Sa(function(o){return AO(1)([u,Wl(.15)(.7)(1500)([Sa(function(f){return kO(1)(fG)([Wl(.4)(.5)(2500)([o,f])])})]),Wl(.29)(.85)(2e3)([Sa(function(f){return AO(1)([Wl(.6)(.6)(3500)([o,Sa(function(p){return kO(1)(lG)([Wl(.75)(.6)(4e3)([f,p]),Wl(.75)(.55)(3e3)([u])])})])])})])])})}})])}}))})}}};var sG=oa(jt)(nt),vG=or(vr),mG=nr({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(Ke()(J)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}));var DG=function(){return D.value}(),hO=function(t){return function(r){return function(e){return function(n){var a=function(u){return sG(n)(vG(r(u))(tn))};return mG(D.value)(DG)({hwLink:a(vc.value)})}}}};var bG=oa(jt)(nt),yG=or(vr),Ac=Q(),AG=nr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(Ac(Ac(Ac(Ac(Ac(Ac(Ac(Ke()(J)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}));var kG=function(){return D.value}(),CO=function(t){return function(r){return function(e){return function(n){var a=function(i){return bG(n)(yG(r(i))(tn))},u=ia(t)(e);return AG(D.value)(kG)({intro:$(hO(t)(r)(e)(n)),next:a(x_.value),code0:$(sO(u)(r)(n)),code1:$(vO(u)(r)(n)),code2:$(mO(u)(r)(n)),code3:$(DO(u)(r)(n)),code4:$(bO(u)(r)(n)),code5:$(gO(u)(r)(n))})}}}};var xO=M(),TO=function(t){return function(r){return new S(V("code")(t)(xO(I(xO(U(z)(r))))))}},kk=TO(w(T));var SO=M(),EO=function(t){return function(r){return new S(V("pre")(t)(SO(I(SO(U(z)(r))))))}},gk=EO(w(T));var xG=or(vr),FO=Q(),TG=nr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(FO(Ke()(FO(J)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})),SG=C(ua),EG=Nt(dt),FG=sc(_f),$G=at(),OG=oa(jt)(nt),wG=function(){return D.value}(),$O=function(t){return function(r){return function(e){return function(n){var a=xG(r(T_.value))(tn),u=ia(t)(e);return TG(D.value)(wG)({code:$(gk([kk([ee(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:$(bt(n)(u)(function(i){return SG(void 0)})(function(i){return function(o){return ut(i)([EG(.15)([FG(440)($G)])])}})),next:OG(n)(a)})}}}};var OO=Vc;var wO=function(){return function(t){return t}};var MO=function(){return function(t){return t}};var hk=function(){function t(){}return t.value=new t,t}();var IO={attr:function(t){return function(r){return d({key:"height",value:P(r)})}}};var Ck=function(){function t(){}return t.value=new t,t}();var PO={attr:function(t){return function(r){return d({key:"width",value:P(r)})}}};var NO=M(),xk=function(t){return function(r){return new S(V("canvas")(t)(NO(I(NO(U(z)(r))))))}};var Tk=function(){function t(){}return t.value=new t,t}();var RO={attr:function(t){return function(r){return d({key:"@self@",value:rt(r)})}}};function Gm(t){return function(){return t.getContext("2d")}}function ns(t){return function(r){return function(){t.fillStyle=r}}}function jm(t){return function(){t.beginPath()}}function Xm(t){return function(){t.fill()}}function Sk(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function Qm(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var VO=Rt(ga),QG=nr({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),KG=vu()(re({reflectSymbol:function(){return"canvas"}})()()(re({reflectSymbol:function(){return"slider"}})()()(pu({reflectSymbol:function(){return"startStop"}})()()()(re({reflectSymbol:function(){return"loading"}})()()(re({reflectSymbol:function(){return"start"}})()()(re({reflectSymbol:function(){return"stop"}})()()(An)()())()())()())(An)()())()())()()),Km=$t(jt),as=C(nt),eu=A(Ct),JO=wr(),GO=St(Or),jO=$r()(),YG=rm(tm(ar(JO(St(GO(Zv)()()()({reflectSymbol:function(){return"fftSize"}}))(Yv)()()()({reflectSymbol:function(){return"cb"}})))(jO))),Ek=C(Yt),ZG=C(Mu),t9=ra(hu),r9=at(),e9=Ma()(gu),ql=ul(ff),LO=Nt(dt),n9=k_(Kb(ar(JO(St(GO(RT)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(Bb)()()()({reflectSymbol:function(){return"delayTime"}})))(jO))),Ym=OA()(gu),Fk=Da(dt),a9=da()(gu),If=w(T),$k=Hn(Gt)(T),u9=kt(PO),o9=kt(IO),i9=kt(H0),BO=kt(RO),Ok=dn(Yt)(Ue),f9=kt(Qo),c9=kt(Ff),l9=kt(Ef),p9=kt(Of),_9=kt(wf),s9=kt(U0),v9=kt($f),m9=hc(Kn),D9=xn(Un),WO=me(Gt)(T),d9=kt(Jc),b9=kt(Se),qO=j(Ct),UO=De(Ct),us=or(vr),y9=Dt(yn),A9=ot(it),Zm=yt(aa),k9=Mn(Ze),g9=Tu(Ze),h9=A(Ci),C9=MO(),x9=LS(Za)(OO),T9=wO(),HO=Ee(Ze),S9=xd(),E9=bn(Do)(Yt),F9=Ua(Yt)(Ue),$9=A(R),O9=A(Ar),w9=vm(sm),M9=mn(fe),I9=function(){return 2*tf}(),Ul=function(t){return{o:t.value0+.04,n:t.value1,t:Ho}};var P9=function(){return D.value}(),N9=function(t){var r=C(t);return function(e){var n=hf(e)(wn);return function(a){return function(u){return r(n({p:[a,u],o:0,d:16}))}}}},Hl=N9(nt)(),R9=function(t){var r=C(t);return function(e){return r(da(e)(wn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},L9=R9(nt)(),B9=function(t){var r=C(t);return function(e){return r(da(e)(wn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}},W9=B9(nt)();var q9=function(t){var r=k_(t);return function(e){var n=Da(e);return function(a){var u=ey(a);return function(i){return function(o){return function(f){return function(p){return function(l){return function(_){return function(m){return r(i)(o)([n(f)(p)([u(l)(_)(m)])])}}}}}}}}}},tD=q9(oo)(dt)(ml),U9=function(t){var r=k_(t);return function(e){var n=Da(e);return function(a){var u=ry(a);return function(i){return function(o){return function(f){return function(p){return function(l){return function(_){return function(m){return r(i)(o)([n(f)(p)([u(l)(_)(m)])])}}}}}}}}}},zO=U9(oo)(dt)(fS),H9=function(t){var r=C(t);return function(e){var n=OA(e)(wn);return function(a){return function(u){return r(n({p:[a,u],o:0,d:16}))}}}},z9=H9(nt)(),XO=400,wk=Vr(XO),V9=function(){return VO(XO)+"px"}(),QO=600,Mk=Vr(QO),J9=function(){return VO(QO)+"px"}(),G9={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},KO=function(t){return function(r){return function(e){return QG(D.value)(P9)({ex1:$(KG(D.value)(function(n){return function(a){var u=Km(as(void 0))(a.startStop.start),i=function(o){return function(f){return function(p){var l=eu(function(_){return new G(_.acTime,_.value)})(Ol(o)(a.slider));return[YG({cb:function(_){return function(){return te(new h(_))(p)(),te(F.value)(p)}},fftSize:Bv.value})(ZG(ea(t9(f)(Km(r9)(eu(function(){var _=ql(fa(0)(.96)(100)(1.04));return function(m){return e9(Ul(_(m)))}}())(l))))(function(_){return function(m){return Sa(function(s){return LO(1)([_,n9({maxDelayTime:2.5,delayTime:1})(eu(function(){var c=ql(fa(0)(.5)(100)(2.45));return function(v){return Ym(Ul(c(v)))}}())(l))([Fk(.4)(eu(function(){var c=ql(fa(0)(.6)(100)(.9));return function(v){return a9(Ul(c(v)))}}())(l))([_])]),tD(.15)(If)(.7)(If)(1500)(Hl(1500)(3e3))([Sa(function(c){return Fk(1)(L9)([tD(.4)(If)(.5)(If)(3e3)(Hl(3e3)(100))([s,c])])})]),tD(.29)(eu(function(){var c=ql(fa(0)(.1)(100)(.4));return function(v){return Ym(Ul(c(v)))}}())(l))(.85)(If)(2e3)(Hl(2e3)(5e3))([Sa(function(c){return LO(1)([tD(.6)(eu(function(){var v=ql(fa(0)(.8)(100)(.3));return function(K){return Ym(Ul(v(K)))}}())(l))(.6)(If)(3500)(Hl(3500)(100))([s,Sa(function(v){return Fk(1)(W9)([zO(.75)(eu(function(){var K=ql(fa(0)(.9)(100)(.1));return function(pt){return Ym(Ul(K(pt)))}}())(l))(.6)(If)(4e3)(Hl(4e3)(200))([c,v]),zO(.75)(z9(.75)(.2))(.55)(If)(200)(Hl(200)(4e3))([_])])})])])})])])})}})))]}}};return Ur([xk(Km($k(as)([u9(Ck.value)(J9),o9(hk.value)(V9),i9(It.value)("width: 100%;"),BO(Tk.value)(function(){var o=Ok(function(f){return function(){var l=Gm(f)();return ns(l)("black")(),Qm(l)({width:Mk,height:wk,x:0,y:0})(),void 0}});return function(f){return o(ub(f))}}())]))(eu(function(o){return BO(Tk.value)(function(){var f=Ok(function(p){return function(){var _=Gm(p)();return ns(_)("black")(),Qm(_)({width:Mk,height:wk,x:0,y:0})(),ns(_)("rgba(255,255,255,0.2)")(),np(o)(function(m){return function(){return jm(_)(),Sk(_)({end:I9,radius:m.value1*40,start:0,x:m.value0.x*Mk,y:m.value0.y*wk,useCounterClockwise:!1})(),Xm(_)()}})()}});return function(p){return f(ub(p))}}())})(a.canvas)))([]),po($k(as)([f9(lo.value)("range"),c9($i.value)("0"),l9(Fi.value)("100"),p9(wi.value)("1"),_9(Mi.value)("50"),s9(It.value)("width: 100%;"),v9(Oi.value)(Kr(function(){var o=Ok(m9(ic)(n.slider)),f=D9(rl);return function(p){return o(f(Ll(p)))}}()))]))([]),on(WO([as(d9(It.value)("width:100%; padding:1.0rem;")),$k(eu(function(){var o=b9(ie.value);return function(f){return o(Kr(g(f)))}}()))([qO(a.startStop.loading)(Ek(void 0)),UO(a.startStop.stop)(function(o){return us(o)(us(t(Ek(void 0)))(n.startStop.start(void 0)))}),UO(y9(qO(u)(A9))(Km(as(Ek(void 0)))(eu(function(o){return o.value0})(e))))(function(o){return function(){o(),n.startStop.loading(void 0)();var p=Nr(F.value)(),l=co(Zm(k9)(function(_){return Zm(g9(_))(function(m){return Zm(h9(C9)(x9(lt(_))(T9(G9))))(function(s){return Zm(HO(Gp(0)(5e4)))(function(c){var v=uf(nb(qn(Yp(s.pluck0))(Qc(Cd(S9(s))))))({newSeed:rf(c),size:4});return HO(function(){var pt=E9(function(Z){return function(){var At=Mo(),jr=Mo();return{x:At,y:jr}}})(Ge(0)(127))(),xt=ut(_)(i(_)(v)(p))(),xr=Pe(mf)(function(Z){return function(){var At=zr(p)();return F9(At)(function(jr){return function(){var fn=$_(jr)(),In=$9(function(){var nu=bp(pt),xe=O9(function(pe){return function(Pa){return Pa/255}(pe)});return function(pe){return nu(xe(pe))}}())(w9(fn))();return n.canvas(In)(),void 0}})()}})(),ce=us(us(us(xt)(m))(M9(_)))(xr);return n.startStop.stop(ce)(),ce})})})})}))();return t(function(){return n.startStop.start(void 0)(),Go(xi(l))()})(),void 0}})])]))([Qe(WO([eu(g("Turn off"))(a.startStop.stop),eu(g("Turn on"))(u),eu(g("Loading..."))(a.startStop.loading)]))])])}}))})}}};var X9=gt({reflectType:function(){return`<div>
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
</div>`}})()()(Q()(Ke()(J)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})),Q9=oa(jt)(nt),K9=or(vr),Y9=function(){return D.value}(),YO=function(t){return function(r){return function(e){return function(n){var a=ia(t)(e);return X9(Y9)({next:Q9(n)(K9(r(vc.value))(tn)),ex:$(KO(a)(r)(n))})}}}};var tj=gt({reflectType:function(){return`<div>
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
</div>`}})()()(Ke()(J)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),rj=C(nt),ej=kt(Se),nj=or(vr),aj=function(){return D.value}(),ZO=function(t){return function(r){return function(e){return function(n){return tj(aj)({next:rj(ej(ie.value)(Kr(g(nj(r(am.value))(tn)))))})}}}};var tw=Q(),oj=gt({reflectType:function(){return`<section>
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
`}})()()(tw(tw(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),ij=Nt(dt),fj=ur(Dr),cj=me(Gt)(T),lj=at(),rw=C(nt),ew=Ma(),pj=ew(wn),_j=so(_o),sj=j(Ar),vj=ew(mT),mj=function(){return D.value}(),nw=function(t){return function(r){return function(e){return oj(mj)({txt:$(ee(`\\ctx buf -> run2 ctx
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
  ]`)),cancel:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([ij(1)([fj(a)(cj([lj,lu(1e3)(rw(pj({p:_j(sj(Ge(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),lu(3e3)(rw(vj({o:3.5})))]))])])}}))})}}};var aw=Q(),dj=gt({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(aw(aw(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})),bj=Nt(dt),yj=ur(Dr),Aj=me(Gt)(T),kj=at(),gj=C(nt),hj=Ma()(wn),Cj=so(_o),xj=j(Ar),Tj=function(){return D.value}(),uw=function(t){return function(r){return function(e){return dj(Tj)({txt:$(ee(`\\ctx buf -> run2 ctx
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
  ]`)),envelope:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([bj(1)([yj(a)(Aj([kj,lu(1e3)(gj(hj({p:Cj(xj(Ge(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}}))})}}};var Ej=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})),Fj=Nt(dt),$j=ur(Dr),rD=Rm(jt),Oj=at(),eD=C(nt),nD=Ma()(gu),wj=function(){return D.value}(),ow=function(t){return function(r){return function(e){return Ej(D.value)(wj)({numericEx:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([Fj(1)([$j(a)(rD(Oj)(function(){return rD(lu(1e3)(rD(eD(nD({n:1,o:1,t:Ib})))(function(){return eD(nD({n:1.3,o:2,t:Ho}))})))(function(){return lu(2500)(rD(eD(nD({n:1,o:2.5,t:Ib})))(function(){return eD(nD({n:.7,o:3.5,t:vT}))}))})}))])])}}))})}}};var Ij=nr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(Q()(J)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})),Pj=Nt(dt),Nj=ur(Dr),Rj=me(Gt)(T),Lj=at(),Bj=C(nt),Wj=Ma()(_T),qj=function(){return D.value}(),iw=function(t){return function(r){return function(e){return Ij(D.value)(qj)({suddenEx:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([Pj(1)([Nj(a)(Rj([Lj,lu(1500)(Bj(Wj({n:1.4})))]))])])}}))})}}};var Hj=gt({reflectType:function(){return`<section>
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
`}})()()(Q()(J)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})),zj=ur(Dr),Vj=me(Gt)(T),Ik=at(),Jj=C(nt),Gj=Ma()(cT(of)(of)),fw=Nt(dt),jj=nm(Jv),Xj=bl(Qv),Qj=A_(cl),Kj=function(){return D.value}(),cw=function(t){return function(r){return function(e){return Hj(Kj)({unitEx:$(bt(e)(t)(function(n){return lt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return ut(n)([zj(a)(Vj([Ik,Jj(Gj(fT(fw(1)([jj(1)(Ik),fw(.2)([Xj(100)([Qj(50)(Ik)])])]))))]))])}}))})}}};var Zj=or(vr),os=Q(),tX=gt({reflectType:function(){return`<div>
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
</div>`}})()()(os(os(Ke()(os(os(os(J)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})),rX=oa(jt)(nt),eX=function(){return D.value}(),lw=function(t){return function(r){return function(e){return function(n){var a=Zj(r(E_.value))(tn),u=ia(t)(e);return tX(eX)({sudden:$(iw(u)(r)(n)),numeric:$(ow(u)(r)(n)),envelope:$(uw(u)(r)(n)),cancel:$(nw(u)(r)(n)),unit:$(cw(u)(r)(n)),next:rX(n)(a)})}}}};var aX=gt({reflectType:function(){return`<div>
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
</div>`}})()()(Ke()(J)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),uX=C(nt),oX=kt(Se),iX=or(vr),fX=function(){return D.value}(),pw=function(t){return function(r){return function(e){return function(n){return aX(fX)({next:uX(oX(ie.value)(Kr(g(iX(r(um.value))(tn)))))})}}}};var lX=gt({reflectType:function(){return`<div>
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
</div>`}})()()(Ke()(J)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})),pX=C(nt),_X=kt(Se),sX=or(vr),vX=function(){return D.value}(),_w=function(t){return function(r){return function(e){return function(n){return lX(vX)({next:pX(_X(ie.value)(Kr(g(sX(r(mc.value))(tn)))))})}}}};var DX=nr({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(J),dX=function(){return D.value}(),sw=function(t){return function(r){return function(e){return function(n){return DX(D.value)(dX)({})}}}};var vw=Q(),yX=nr({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(vw(vw(J)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})),AX=vu()(re({reflectSymbol:function(){return"slider"}})()()(pu({reflectSymbol:function(){return"startStop"}})()()()(re({reflectSymbol:function(){return"loading"}})()()(re({reflectSymbol:function(){return"start"}})()()(re({reflectSymbol:function(){return"stop"}})()()(An)()())()())()())(An)()())()()),mw=$t(jt),is=C(nt),kX=Si(Ct),gX=fu($e),hX=Nt(dt),fs=A(Ct),Dw=me(Gt)(T),CX=ra(D_(ar(wr()(St(St(Or)(xT)()()()({reflectSymbol:function(){return"playbackRate"}}))(m_)()()()({reflectSymbol:function(){return"buffer"}})))($r()()))),xX=at(),dw=Hn(Gt)(T),TX=kt(Qo),SX=kt(Ff),EX=kt(Ef),FX=kt(Of),$X=kt(wf),OX=kt($f),wX=kt(Se),bw=j(Ct),Pk=C(Yt),yw=De(Ct),aD=or(vr),MX=Dt(yn),IX=ot(it),Nk=yt(aa),PX=Mn(Ze),NX=Tu(Ze),RX=Ee(Ze),LX=mn(fe),BX=`module Main where

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
`,WX=Ia(function(t){return Yn(function(r){return Pe(t)(function(e){return function(){var a=Mo();return r(e(a))()}})})}),qX=function(){return D.value}(),UX="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",Aw=function(t){return function(r){return function(e){return yX(D.value)(qX)({txt:$(ee(BX)),ex1:$(AX(D.value)(function(n){return function(a){var u=mw(is(void 0))(a.startStop.start),i=kX(G.create)(WX)(gX(function(f){return function(p){return f+1|0}})(0)(a.slider)),o=function(f){return[hX(1)([Rp(fs(function(p){return Dw([is(oT(CX({buffer:f,playbackRate:.7+_a(p)*2})(xX))),lu(5e3)(is(iT))])})(i))])]};return Ur([Ur([ee("Slide me!"),po(dw(is)([TX(lo.value)("range"),SX($i.value)("0"),EX(Fi.value)("100"),FX(wi.value)("1"),$X(Mi.value)("50"),OX(Oi.value)(Kr(g(n.slider(void 0))))]))([])]),on(dw(fs(function(){var f=wX(ie.value);return function(p){return f(Kr(g(p)))}}()))([bw(a.startStop.loading)(Pk(void 0)),yw(a.startStop.stop)(function(f){return aD(f)(aD(t(Pk(void 0)))(n.startStop.start(void 0)))}),yw(MX(bw(u)(IX))(mw(is(Pk(void 0)))(fs(function(f){return f.value0})(e))))(function(f){return function(){f(),n.startStop.loading(void 0)();var l=co(Nk(PX)(function(_){return Nk(NX(_))(function(m){return Nk(lt(_)(UX))(function(s){return RX(function(){var v=_m(o(s))(),K=aD(aD(v)(m))(LX(_));return n.startStop.stop(K)(),K})})})}))();return t(function(){return n.startStop.start(void 0)(),Go(xi(l))()})(),void 0}})]))([Qe(Dw([fs(g("Turn off"))(a.startStop.stop),fs(g("Turn on"))(u)]))])])}}))})}}};var kw=Q(),zX=gt({reflectType:function(){return`<div>
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
</div>`}})()()(kw(kw(J)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})),VX=Nt(dt),JX=ur(Dr),GX=at(),jX=function(){return D.value}(),gw=function(t){return function(r){return function(e){return function(n){var a=ia(t)(e);return zX(jX)({appl:$(pm("\u{1F44F}")(n)(a)(function(u){return lt(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(i){return ut(u)([VX(1)([JX(i)(GX)])])}})),suby:$(Aw(a)(r)(n))})}}}};var Fu=C(Mu),QX=fu($e),KX=C(Yt),YX=A(Ar),ZX=$t(jt),tQ=Hn(Gt)(T),hw=C(nt),Cw=kt(Se),rQ=kt(V0),eQ=A(Ct),xw=xp(cu),nQ=kt(Zs),Tw=pr(mD(Hf(Sc))),UXt=function(t){return t},HXt={Coercible0:function(){}},aQ=function(){var t=function(r){var e=function(n){if(n instanceof C_)return Ur(Fu(He(YO(r.setCancellation)(r.setPage))));if(n instanceof vc)return Ur(Fu(He($O(r.setCancellation)(r.setPage))));if(n instanceof T_)return Ur(Fu(He(CO(r.setCancellation)(r.setPage))));if(n instanceof x_)return Ur(Fu(He(gF(r.setCancellation)(r.setPage))));if(n instanceof am)return Ur(Fu(He(_w(r.setCancellation)(r.setPage))));if(n instanceof mc)return Ur(Fu(He(_O(r.setCancellation)(r.setPage))));if(n instanceof S_)return Ur(Fu(He(lw(r.setCancellation)(r.setPage))));if(n instanceof E_)return Ur(Fu(He(I$(r.setCancellation)(r.setPage))));if(n instanceof um)return Ur(Fu(He(sw(r.setCancellation)(r.setPage))));if(n instanceof rE)return Ur(Fu(He(ZO(r.setCancellation)(r.setPage))));if(n instanceof F_)return Ur(Fu(He(gw(r.setCancellation)(r.setPage))));if(n instanceof eE)return Ur(Fu(He(pw(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 148, column 5 - line 148, column 76): "+[n.constructor.name])};return e(r.page)};return K0(Q0(new hl(C_.value)))(function(r){var e=QX(lr(function(n){if(n instanceof hl)return function(a){return{prevPage:new h(a.curPage),curPage:n.value0,cancel:a.cancel,pageChange:!0}};if(n instanceof iy)return function(a){return{cancel:n.value0,pageChange:!1,curPage:a.curPage,prevPage:a.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 59, column 13 - line 61, column 81): "+[n.constructor.name])}))({prevPage:F.value,curPage:C_.value,cancel:KX(void 0),pageChange:!0})(r.value1);return Ur([Ur(YX(function(n){return Ld([Rd(ZX(tQ(hw)([Cw(ie.value)(Kr(g(r.value0(new hl(n.value0))))),rQ(It.value)("cursor:pointer;")]))(eQ(function(a){return Cw(ie.value)(Kr(g(function(){return a.cancel(),r.value0(new hl(n.value0))()})))})(xw(function(a){return!function(u){return u.pageChange}(a)})(e))))([ee(n.value1.value0)]),jc(hw(nQ(It.value)(function(){return n.value1.value1?"":"display:none;"}())))([ee(" | ")])])})([new G(C_.value,new G("Home",!0)),new G(vc.value,new G("Hello world",!0)),new G(T_.value,new G("Array, fan, and fix",!0)),new G(x_.value,new G("Audio units",!0)),new G(mc.value,new G("Events",!0)),new G(S_.value,new G("Parameters",!0)),new G(E_.value,new G("State",!0)),new G(F_.value,new G("Subgraphs",!1))])),B0(function(n){return t({page:n.curPage,setPage:function(a){return r.value0(hl.create(a))},setCancellation:function(a){return r.value0(iy.create(a))}})})(xw(function(n){return n.pageChange})(e))])})}(),zXt=function(t){return{page:t,setPage:Tw,setCancellation:Tw}},VXt=ox(aQ);export{UXt as TopLevelSg,VXt as main,HXt as newtypeTopLevelSg_,zXt as p2tl,aQ as scene};
