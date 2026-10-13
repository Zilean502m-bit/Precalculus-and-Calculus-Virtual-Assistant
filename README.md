// =====================================================================
//  CalcuGuia - Asistente virtual de Calculo y Precalculo (C++17)
//  Universidad Mariano Galvez de Guatemala - Facultad de Ingenieria
//  Compilar:  g++ -std=c++17 -O2 -Wno-misleading-indentation -o calcuguia calcuguia.cpp
//  Sin librerias externas: analizador de expresiones, derivacion
//  simbolica, integracion, limites, graficas (ASCII + SVG) y practica.
// =====================================================================
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#ifdef _WIN32
#include <windows.h>
#endif
using namespace std;

// ---------- DATOS DE LOS INTEGRANTES (EDITAR) ----------
const string INTEGRANTES[2] = {"Nombre del estudiante 1", "Nombre del estudiante 2"};
const string ASIGNATURA = "Calculo y Precalculo", SECCION = "Seccion __";

const double PI = 3.14159265358979323846, E_ = 2.71828182845904523536;
struct UErr : runtime_error { using runtime_error::runtime_error; };
[[noreturn]] void fail(const string& m) { throw UErr(m); }

string fnum(double v) {
    if (isnan(v)) return "indefinido";
    if (isinf(v)) return v > 0 ? "+inf" : "-inf";
    char b[64]; snprintf(b, sizeof b, "%.6f", v); string s = b;
    if (s.find('.') != string::npos) { while (s.back() == '0') s.pop_back(); if (s.back() == '.') s.pop_back(); }
    return (s == "-0" || s.empty()) ? "0" : s;
}
string rat(double v) {  // entero o fraccion p/q
    double r = round(v); if (fabs(v - r) < 1e-9) return fnum(r);
    for (int q = 2; q <= 24; q++) { double p = round(v * q); if (fabs(v - p / q) < 1e-9) return fnum(p) + "/" + to_string(q); }
    return fnum(v);
}
double cl(double x) { return fabs(x - round(x)) < 1e-6 ? round(x) : x; }

// ---------- ARBOL DE EXPRESIONES ----------
enum T { NUM, VAR, ADD, SUB, MUL, DIV, POW, NEG, FN };
struct N; using P = shared_ptr<N>;
struct N { T t; double v = 0; string f; P a, b; };
P num(double v) { auto n = make_shared<N>(); n->t = NUM; n->v = v; return n; }
P var() { auto n = make_shared<N>(); n->t = VAR; return n; }
P mk(T t, P a, P b = nullptr) { auto n = make_shared<N>(); n->t = t; n->a = a; n->b = b; return n; }
P fn(const string& f, P a) { auto n = make_shared<N>(); n->t = FN; n->f = f; n->a = a; return n; }

struct Parser {
    string s; size_t i = 0;
    explicit Parser(const string& str) {
        for (size_t k = 0; k < str.size(); k++) {
            char c = tolower((unsigned char)str[k]);
            if (c == ',' && k > 0 && k + 1 < str.size() && isdigit((unsigned char)str[k - 1]) && isdigit((unsigned char)str[k + 1])) c = '.';
            if (c == '*' && k + 1 < str.size() && str[k + 1] == '*') { s += '^'; k++; continue; }
            if (c == '[' ) c = '('; if (c == ']') c = ')';
            s += c;
        }
    }
    void ws() { while (i < s.size() && isspace((unsigned char)s[i])) i++; }
    bool eat(char c) { ws(); if (i < s.size() && s[i] == c) { i++; return true; } return false; }
    P parse() {
        ws(); if (i >= s.size()) fail("La expresion esta vacia.");
        P r = expr(); ws();
        if (i < s.size()) fail(string("Caracter inesperado '") + s[i] + "' en la posicion " + to_string(i + 1) + ".");
        return r;
    }
    P expr() { P l = term(); for (;;) { if (eat('+')) l = mk(ADD, l, term()); else if (eat('-')) l = mk(SUB, l, term()); else break; } return l; }
    P term() {
        P l = unary();
        for (;;) {
            ws();
            if (eat('*')) l = mk(MUL, l, unary());
            else if (eat('/')) l = mk(DIV, l, unary());
            else if (i < s.size() && (isalpha((unsigned char)s[i]) || s[i] == '(')) l = mk(MUL, l, power());
            else break;
        }
        return l;
    }
    P unary() { if (eat('-')) return mk(NEG, unary()); if (eat('+')) return unary(); return power(); }
    P power() { P b = atom(); if (eat('^')) return mk(POW, b, unary()); return b; }
    P atom() {
        ws(); if (i >= s.size()) fail("La expresion esta incompleta.");
        char c = s[i];
        if (isdigit((unsigned char)c) || c == '.') {
            size_t j = i; int dots = 0;
            while (j < s.size() && (isdigit((unsigned char)s[j]) || s[j] == '.')) { if (s[j] == '.') dots++; j++; }
            if (dots > 1) fail("Numero mal escrito: " + s.substr(i, j - i));
            double v = stod(s.substr(i, j - i)); i = j; return num(v);
        }
        if (c == '(') { i++; P e = expr(); if (!eat(')')) fail("Falta cerrar un parentesis."); return e; }
        if (isalpha((unsigned char)c)) {
            size_t j = i; while (j < s.size() && isalpha((unsigned char)s[j])) j++;
            string id = s.substr(i, j - i); i = j;
            if (id == "x") return var(); if (id == "pi") return num(PI); if (id == "e") return num(E_);
            if (id == "sen") id = "sin"; if (id == "ln") id = "log";
            static const set<string> F = {"sin", "cos", "tan", "exp", "log", "sqrt", "abs", "asin", "atan"};
            if (F.count(id)) {
                ws(); if (!eat('(')) fail("Escriba la funcion con parentesis, por ejemplo " + id + "(x).");
                P a = expr(); if (!eat(')')) fail("Falta cerrar el parentesis de " + id + ".");
                return fn(id, a);
            }
            fail("Simbolo no reconocido '" + id + "'. Use solo la variable x y funciones: sin cos tan exp log sqrt abs asin atan (escriba x*sin(x), no xsin(x)).");
        }
        fail(string("Caracter no valido '") + c + "'.");
    }
};
P parse(const string& s) { return Parser(s).parse(); }

bool hasX(const P& n) { if (!n) return false; if (n->t == VAR) return true; return hasX(n->a) || hasX(n->b); }
double ev(const P& n, double x) {
    switch (n->t) {
    case NUM: return n->v; case VAR: return x;
    case ADD: return ev(n->a, x) + ev(n->b, x); case SUB: return ev(n->a, x) - ev(n->b, x);
    case MUL: return ev(n->a, x) * ev(n->b, x); case DIV: return ev(n->a, x) / ev(n->b, x);
    case POW: return pow(ev(n->a, x), ev(n->b, x)); case NEG: return -ev(n->a, x);
    case FN: { double u = ev(n->a, x); const string& f = n->f;
        if (f == "sin") return sin(u); if (f == "cos") return cos(u); if (f == "tan") return tan(u);
        if (f == "exp") return exp(u); if (f == "log") return u > 0 ? log(u) : NAN; if (f == "sqrt") return u >= 0 ? sqrt(u) : NAN;
        if (f == "abs") return fabs(u); if (f == "asin") return asin(u); return atan(u); }
    }
    return NAN;
}
void walk(const P& n, const function<void(const P&)>& g) { if (!n) return; g(n); walk(n->a, g); walk(n->b, g); }

// ---------- IMPRESION ----------
int precN(const P& n) {
    switch (n->t) { case ADD: case SUB: return 1; case MUL: case DIV: return 2; case NEG: return 3; case POW: return 4;
    case NUM: return n->v < 0 ? 1 : 5; default: return 5; }
}
string str(const P& n, int pp = 0) {
    string s;
    switch (n->t) {
    case NUM: s = fnum(n->v); break; case VAR: s = "x"; break;
    case ADD: s = str(n->a, 1) + " + " + str(n->b, 1); break; case SUB: s = str(n->a, 1) + " - " + str(n->b, 2); break;
    case MUL: s = str(n->a, 2) + "*" + str(n->b, 2); break; case DIV: s = str(n->a, 2) + "/" + str(n->b, 3); break;
    case NEG: s = "-" + str(n->a, 3); break; case POW: s = str(n->a, 5) + "^" + str(n->b, 5); break;
    case FN: s = n->f + "(" + str(n->a, 0) + ")"; break;
    }
    return precN(n) < pp ? "(" + s + ")" : s;
}
bool isn(const P& n, double v) { return n->t == NUM && n->v == v; }
P simp(const P& n) {
    if (!n || n->t == NUM || n->t == VAR) return n;
    P a = simp(n->a), b = n->b ? simp(n->b) : nullptr;
    if (n->t == FN) return fn(n->f, a);
    if (n->t == NEG) { if (a->t == NUM) return num(-a->v); if (a->t == NEG) return a->a; return mk(NEG, a); }
    if (a->t == NUM && b->t == NUM) {
        double x = a->v, y = b->v;
        switch (n->t) { case ADD: return num(x + y); case SUB: return num(x - y); case MUL: return num(x * y);
        case DIV: if (y != 0) return num(x / y); break; case POW: { double r = pow(x, y); if (isfinite(r)) return num(r); break; } default: break; }
    }
    switch (n->t) {
    case ADD: if (isn(a, 0)) return b; if (isn(b, 0)) return a; if (b->t == NUM && b->v < 0) return simp(mk(SUB, a, num(-b->v))); if (b->t == NEG) return mk(SUB, a, b->a); break;
    case SUB: if (isn(b, 0)) return a; if (isn(a, 0)) return simp(mk(NEG, b)); if (b->t == NUM && b->v < 0) return simp(mk(ADD, a, num(-b->v))); if (b->t == NEG) return mk(ADD, a, b->a); break;
    case MUL:
        if (isn(a, 0) || isn(b, 0)) return num(0); if (isn(a, 1)) return b; if (isn(b, 1)) return a;
        if (isn(a, -1)) return simp(mk(NEG, b)); if (isn(b, -1)) return simp(mk(NEG, a));
        if (b->t == NUM && a->t != NUM) return mk(MUL, b, a);
        if (a->t == NUM && b->t == MUL && b->a->t == NUM) return simp(mk(MUL, num(a->v * b->a->v), b->b));
        if (a->t == NEG) return simp(mk(NEG, mk(MUL, a->a, b))); if (b->t == NEG) return simp(mk(NEG, mk(MUL, a, b->a))); break;
    case DIV: if (isn(b, 1)) return a; if (isn(a, 0)) return num(0); if (a->t == NEG) return simp(mk(NEG, mk(DIV, a->a, b))); break;
    case POW: if (isn(b, 1)) return a; if (isn(b, 0)) return num(1); if (isn(a, 1)) return num(1); break;
    default: break;
    }
    return mk(n->t, a, b);
}
P sub(const P& n, const P& g) {  // sustituye x por g
    if (!n) return n; if (n->t == VAR) return g; if (n->t == NUM) return n;
    auto m = make_shared<N>(*n); m->a = sub(n->a, g); m->b = sub(n->b, g); return m;
}

// ---------- DERIVADA SIMBOLICA ----------
P d(const P& n) {
    switch (n->t) {
    case NUM: return num(0); case VAR: return num(1);
    case ADD: return mk(ADD, d(n->a), d(n->b)); case SUB: return mk(SUB, d(n->a), d(n->b)); case NEG: return mk(NEG, d(n->a));
    case MUL: return mk(ADD, mk(MUL, d(n->a), n->b), mk(MUL, n->a, d(n->b)));
    case DIV: return mk(DIV, mk(SUB, mk(MUL, d(n->a), n->b), mk(MUL, n->a, d(n->b))), mk(POW, n->b, num(2)));
    case POW:
        if (!hasX(n->b)) return mk(MUL, mk(MUL, n->b, mk(POW, n->a, mk(SUB, n->b, num(1)))), d(n->a));
        if (!hasX(n->a)) return mk(MUL, mk(MUL, n, fn("log", n->a)), d(n->b));
        return mk(MUL, n, mk(ADD, mk(MUL, d(n->b), fn("log", n->a)), mk(DIV, mk(MUL, n->b, d(n->a)), n->a)));
    case FN: { P u = n->a, du = d(u); const string& f = n->f;
        if (f == "sin") return mk(MUL, fn("cos", u), du); if (f == "cos") return mk(NEG, mk(MUL, fn("sin", u), du));
        if (f == "tan") return mk(DIV, du, mk(POW, fn("cos", u), num(2))); if (f == "exp") return mk(MUL, n, du);
        if (f == "log") return mk(DIV, du, u); if (f == "sqrt") return mk(DIV, du, mk(MUL, num(2), n));
        if (f == "abs") return mk(DIV, mk(MUL, u, du), n);
        if (f == "asin") return mk(DIV, du, fn("sqrt", mk(SUB, num(1), mk(POW, u, num(2)))));
        return mk(DIV, du, mk(ADD, num(1), mk(POW, u, num(2)))); }
    }
    return num(0);
}
P der(const P& n) { return simp(simp(d(n))); }
vector<string> rules(const P& f) {
    set<string> S;
    walk(f, [&](const P& q) {
        auto xa = [&]() { return (int)hasX(q->a) + (int)hasX(q->b); };
        if (q->t == ADD || q->t == SUB) S.insert("Suma/resta: se deriva termino a termino.");
        if (q->t == MUL) S.insert(xa() > 1 ? "Producto: (u*v)' = u'v + uv'." : "Constante por funcion: (k*u)' = k*u'.");
        if (q->t == DIV) S.insert(hasX(q->b) ? "Cociente: (u/v)' = (u'v - uv')/v^2." : "Division entre constante.");
        if (q->t == POW) S.insert(hasX(q->b) ? "Exponencial general: (u^v)' = u^v (v' ln u + v u'/u)." : "Potencia: (x^n)' = n*x^(n-1).");
        if (q->t == FN) {
            static map<string, string> M = {{"sin", "(sen u)' = cos u * u'"}, {"cos", "(cos u)' = -sen u * u'"}, {"tan", "(tan u)' = u'/cos^2 u"}, {"exp", "(e^u)' = e^u * u'"}, {"log", "(ln u)' = u'/u"}, {"sqrt", "(raiz u)' = u'/(2 raiz u)"}, {"abs", "(|u|)' = u*u'/|u|"}};
            S.insert("Funcion elemental: " + (M.count(q->f) ? M[q->f] : q->f + "'"));
            if (q->a->t != VAR) S.insert("Regla de la cadena: [f(u)]' = f'(u)*u'.");
        }
    });
    return vector<string>(S.begin(), S.end());
}

// ---------- POLINOMIOS ----------
typedef vector<double> Pol;
void trimP(Pol& c) { while (c.size() > 1 && fabs(c.back()) < 1e-12) c.pop_back(); if (c.empty()) c = {0}; }
Pol mulP(const Pol& u, const Pol& v) { Pol r(u.size() + v.size() - 1, 0); for (size_t i = 0; i < u.size(); i++) for (size_t j = 0; j < v.size(); j++) r[i + j] += u[i] * v[j]; return r; }
bool poly(const P& n, Pol& c) {
    switch (n->t) {
    case NUM: c = {n->v}; return true; case VAR: c = {0, 1}; return true;
    case NEG: if (!poly(n->a, c)) return false; for (auto& v : c) v = -v; return true;
    case ADD: case SUB: { Pol u, v; if (!poly(n->a, u) || !poly(n->b, v)) return false; if (v.size() > u.size()) u.resize(v.size(), 0);
        for (size_t i = 0; i < v.size(); i++) u[i] += n->t == ADD ? v[i] : -v[i]; c = u; trimP(c); return true; }
    case MUL: { Pol u, v; if (!poly(n->a, u) || !poly(n->b, v)) return false; c = mulP(u, v); trimP(c); return true; }
    case DIV: { if (hasX(n->b)) return false; Pol u; if (!poly(n->a, u)) return false; double q = ev(n->b, 0); if (q == 0 || !isfinite(q)) return false; for (auto& v : u) v /= q; c = u; return true; }
    case POW: { if (hasX(n->b)) return false; double k = ev(n->b, 0); if (!(k >= 0 && k <= 30) || fabs(k - round(k)) > 1e-12) return false;
        Pol u; if (!poly(n->a, u)) return false; c = {1}; for (int i = 0; i < (int)round(k); i++) c = mulP(c, u); trimP(c); return true; }
    default: return false;
    }
}
string pstr(const Pol& c) {
    string s;
    for (int k = (int)c.size() - 1; k >= 0; k--) {
        double v = c[k]; if (fabs(v) < 1e-12 && c.size() > 1) continue;
        string t = fnum(fabs(v)); bool one = fabs(fabs(v) - 1) < 1e-12 && k > 0;
        string term = k == 0 ? t : (one ? "" : t + "*") + "x" + (k > 1 ? "^" + to_string(k) : "");
        if (s.empty()) s = (v < 0 ? "-" : "") + term; else s += (v < 0 ? " - " : " + ") + term;
    }
    return s.empty() ? "0" : s;
}

// ---------- NUMERICA ----------
typedef function<double(double)> Fn;
vector<double> roots(const Fn& g, double a, double b, int n = 6000) {
    double h = (b - a) / n; vector<double> xs(n + 1), ys(n + 1), r;
    for (int i = 0; i <= n; i++) { xs[i] = a + i * h; ys[i] = g(xs[i]); }
    auto add = [&](double x) { for (double q : r) if (fabs(q - x) < 1e-5) return; r.push_back(x); };
    for (int i = 0; i < n; i++) {
        double y0 = ys[i], y1 = ys[i + 1]; if (!isfinite(y0) || !isfinite(y1)) continue;
        if (y0 == 0) { add(xs[i]); continue; }
        if (y0 * y1 < 0) {
            double lo = xs[i], hi = xs[i + 1], fl = y0;
            for (int k = 0; k < 70; k++) { double m = (lo + hi) / 2, fm = g(m); if (fl * fm <= 0) hi = m; else { lo = m; fl = fm; } }
            double m = (lo + hi) / 2; if (fabs(g(m)) < 1e-6) add(m);
        } else if (i > 0 && isfinite(ys[i - 1])) {
            double A = fabs(ys[i - 1]), B = fabs(y0), C = fabs(y1);
            if (B <= A && B < C && B < 1e-3) {
                double lo = xs[i - 1], hi = xs[i + 1];
                for (int k = 0; k < 80; k++) { double m1 = lo + (hi - lo) / 3, m2 = hi - (hi - lo) / 3; if (fabs(g(m1)) < fabs(g(m2))) hi = m2; else lo = m1; }
                double m = (lo + hi) / 2; if (fabs(g(m)) < 1e-6) add(m);
            }
        }
    }
    if (isfinite(ys[n]) && ys[n] == 0) add(xs[n]);
    sort(r.begin(), r.end()); return r;
}
double simpson(const Fn& f, double a, double b, int n = 2000) { double h = (b - a) / n, s = f(a) + f(b); for (int i = 1; i < n; i++) s += f(a + i * h) * (i % 2 ? 4 : 2); return s * h / 3; }
double dnum(const Fn& f, double x) { return (f(x + 1e-5) - f(x - 1e-5)) / 2e-5; }
Fn mkf(const P& n) { return [n](double x) { return ev(n, x); }; }
double numv(const string& s) {
    P e = parse(s); if (hasX(e)) fail("Se esperaba un numero, no una expresion con x: '" + s + "'.");
    double v = ev(e, 0); if (!isfinite(v)) fail("Valor numerico no valido: '" + s + "'."); return v;
}
vector<double> listv(const string& s) {
    vector<double> r; string t; for (char c : s + ";") { if (c == ',' || c == ';' || c == ' ') { if (!t.empty()) r.push_back(numv(t)); t.clear(); } else t += c; } return r;
}

// ---------- INFORME ----------
void rep(const string& formula, const string& subst, const vector<string>& proc, const string& ans, const vector<string>& warn = {}) {
    cout << "\n=== FORMULA / METODO ===\n" << formula << "\n--- SUSTITUCION DE VALORES ---\n" << subst << "\n--- PROCEDIMIENTO ---\n";
    int k = 1; for (auto& p : proc) cout << " " << k++ << ". " << p << "\n";
    cout << "--- RESPUESTA FINAL ---\n >> " << ans << "\n";
    for (auto& w : warn) cout << " [AVISO] " << w << "\n";
}

// ---------- ANALISIS (extremos, crecimiento) ----------
struct Crit { double x, y, s; string t; };
struct An { P d1, d2; vector<double> rs; vector<Crit> pts; double yi; vector<string> iv; };
An analyze(const P& f, double a, double b) {
    An r; r.d1 = der(f); r.d2 = der(r.d1); Fn F = mkf(f), g1 = mkf(r.d1), g2 = mkf(r.d2);
    r.rs = roots(F, a, b); r.yi = F(0);
    for (double x : roots(g1, a, b, 8000)) {
        x = cl(x); if (!isfinite(F(x))) continue; double s = g2(x); string t;
        if (fabs(s) > 1e-6) t = s > 0 ? "minimo relativo" : "maximo relativo";
        else { double l = g1(x - 1e-3), rr = g1(x + 1e-3); t = (l < 0 && rr > 0) ? "minimo relativo" : (l > 0 && rr < 0) ? "maximo relativo" : "ni maximo ni minimo (inflexion)"; }
        r.pts.push_back({x, F(x), s, t});
    }
    vector<double> xs = {a}; for (auto& q : r.pts) if (q.x > a && q.x < b) xs.push_back(q.x); xs.push_back(b);
    for (size_t i = 0; i + 1 < xs.size(); i++) { double m = g1((xs[i] + xs[i + 1]) / 2);
        r.iv.push_back("(" + fnum(xs[i]) + ", " + fnum(xs[i + 1]) + "): " + (!isfinite(m) ? "no definido" : m > 0 ? "creciente" : m < 0 ? "decreciente" : "constante")); }
    return r;
}

// ================= MODULO: ALGEBRA =================
void solveEq(const string& in) {
    string s = in; size_t p = s.find('='); if (p != string::npos && s.find('=', p + 1) != string::npos) fail("La ecuacion debe tener un solo signo '='.");
    string L = p == string::npos ? s : s.substr(0, p), R = p == string::npos ? "0" : s.substr(p + 1);
    if (L.find_first_not_of(' ') == string::npos || R.find_first_not_of(' ') == string::npos) fail("Falta un lado de la ecuacion.");
    P f = simp(mk(SUB, parse(L), parse(R))); Fn g = mkf(f); vector<string> pr = {"Pasar todo a un lado: f(x) = (" + L + ") - (" + R + ") = 0."}, w; vector<double> rs; string formula, subst;
    Pol c; bool isP = poly(f, c); int deg = isP ? (int)c.size() - 1 : -1; string ans;
    if (isP && deg == 0) fail(fabs(c[0]) < 1e-12 ? "La ecuacion es una identidad (infinitas soluciones)." : "La ecuacion no tiene solucion (contradiccion).");
    if (isP && deg == 1) { formula = "ax + b = 0  =>  x = -b/a"; subst = "a = " + fnum(c[1]) + ", b = " + fnum(c[0]) + "  =>  x = -(" + fnum(c[0]) + ")/" + fnum(c[1]); rs = {-c[0] / c[1]}; pr.push_back("Forma: " + pstr(c) + " = 0, despejar x."); }
    else if (isP && deg == 2) {
        double A = c[2], B = c[1], C = c[0], D = B * B - 4 * A * C; formula = "x = (-b +- raiz(b^2 - 4ac)) / (2a)";
        subst = "a = " + fnum(A) + ", b = " + fnum(B) + ", c = " + fnum(C) + "  =>  x = (-(" + fnum(B) + ") +- raiz(" + fnum(D) + ")) / (2*" + fnum(A) + ")";
        pr.push_back("Ecuacion cuadratica: " + pstr(c) + " = 0.");
        pr.push_back("Discriminante D = b^2 - 4ac = (" + fnum(B) + ")^2 - 4(" + fnum(A) + ")(" + fnum(C) + ") = " + fnum(D) + (D > 1e-12 ? " > 0: dos raices reales." : fabs(D) <= 1e-12 ? " = 0: raiz real doble." : " < 0: raices complejas."));
        if (D >= -1e-12) { double q = sqrt(max(D, 0.0)); rs = {(-B - q) / (2 * A), (-B + q) / (2 * A)}; if (fabs(D) <= 1e-12) rs.pop_back(); sort(rs.begin(), rs.end()); }
        else { ans = "x = " + fnum(-B / (2 * A)) + " +- " + fnum(sqrt(-D) / fabs(2 * A)) + " i  (soluciones complejas, sin soluciones reales)"; }
    } else {
        formula = isP ? "Ecuacion polinomial de grado " + to_string(deg) + ": se buscan los ceros de f(x) (factorizacion / teorema del factor / metodo numerico)." : "Se buscan los ceros de f(x) (metodo numerico: cambio de signo + biseccion).";
        subst = "f(x) = " + str(f); double B = 1000; if (isP) { double m = 0; for (int k = 0; k < deg; k++) m = max(m, fabs(c[k] / c[deg])); B = min(1 + m, 1e5); }
        rs = roots(g, -B, B, 60000); for (auto& r : rs) r = cl(r); pr.push_back("Se exploran cambios de signo de f en [" + fnum(-B) + ", " + fnum(B) + "] y se refinan por biseccion.");
        if (str(f).find('/') != string::npos) w.push_back("Ecuacion racional: se descartaron valores que anulan un denominador.");
    }
    if (ans.empty()) { for (double r : rs) ans += (ans.empty() ? "" : ";  ") + string("x = ") + rat(cl(r)) + (rat(cl(r)).find('/') != string::npos ? " = " + fnum(r) : ""); if (ans.empty()) ans = "Sin soluciones reales"; }
    string v; for (double r : rs) v += "f(" + fnum(cl(r)) + ") = " + fnum(fabs(g(r)) < 1e-9 ? 0 : g(r)) + "; "; pr.push_back("Verificacion (sustituir en f): " + (v.empty() ? string("-") : v));
    rep(formula, subst, pr, ans, w);
}
void factorExpr(const string& in) {
    P f = parse(in); Pol c; if (!poly(f, c)) fail("Solo se factorizan polinomios en x con coeficientes numericos.");
    int deg = (int)c.size() - 1; if (deg < 1) fail("Ingrese un polinomio de grado 1 o mayor.");
    double B = 1; for (int k = 0; k < deg; k++) B = max(B, 1 + fabs(c[k] / c[deg])); B = min(B, 1e5);
    vector<double> rr = roots(mkf(f), -B, B, 60000); Pol cur = c; map<double, int> mult; double scale = 0; for (double v : c) scale = max(scale, fabs(v));
    for (double r : rr) for (int q = 1; q <= 12; q++) {
        double pn = round(r * q); if (fabs(r * q - pn) > 1e-7 * q) continue; double R = pn / q;
        while (cur.size() > 1) {
            double val = 0; for (int k = (int)cur.size() - 1; k >= 0; k--) val = val * R + cur[k];
            if (fabs(val) > 1e-7 * (1 + scale)) break;
            Pol nq(cur.size() - 1); double carry = 0; for (int k = (int)cur.size() - 1; k >= 1; k--) { carry = cur[k] + carry * R; nq[k - 1] = carry; }
            cur = nq; mult[R]++;
        }
        break;
    }
    string fs; double lead = c.back();
    if (fabs(lead - 1) > 1e-12) fs += fabs(lead + 1) < 1e-12 ? "-" : fnum(lead) + "*";
    vector<string> rt; for (auto& kv : mult) { string t = "(x " + string(kv.first < 0 ? "+ " : "- ") + rat(fabs(kv.first)) + ")" + (kv.second > 1 ? "^" + to_string(kv.second) : ""); fs += t; rt.push_back(rat(kv.first)); }
    string irr; if (cur.size() > 1) { Pol m = cur; double l = m.back(); for (auto& v : m) v /= l; irr = "(" + pstr(m) + ")"; fs += irr; }
    if (mult.empty()) fs = in + "  ya esta factorizado o es irreducible en los racionales";
    vector<string> w; Pol dummy; double err = 0; for (double x : {-3.3, 0.7, 2.9, 5.1}) { double pv = lead; for (auto& kv : mult) pv *= pow(x - kv.first, kv.second); if (cur.size() > 1) { double t = 0; for (int k = (int)cur.size() - 1; k >= 0; k--) t = t * x + cur[k]; pv *= t / cur.back(); } err = max(err, fabs(pv - ev(f, x)) / (1 + fabs(ev(f, x)))); }
    if (err > 1e-6) w.push_back("La verificacion numerica no coincide; revise la expresion.");
    rep("Factor comun, a^2-b^2=(a-b)(a+b), x^2+bx+c=(x+m)(x+n), Teorema del factor: (x-r) es factor <=> f(r)=0",
        "f(x) = " + pstr(c), {"Se busca factor comun / coeficiente principal: " + fnum(lead) + ".", "Raices racionales halladas (teorema del factor): " + (rt.empty() ? string("ninguna") : [&] { string s; for (auto& r : rt) s += r + " "; return s; }()), "Se divide (division sintetica) por cada (x - r) encontrado.", cur.size() > 1 ? "El factor restante " + irr + " no tiene raices racionales." : "No queda factor restante.", "Verificacion numerica en 4 puntos: " + string(err <= 1e-6 ? "coincide" : "NO coincide")}, fs, w);
}
void domRange(const string& in) {
    P f = parse(in); vector<string> res; walk(f, [&](const P& q) {
        if (q->t == DIV) res.push_back("Denominador: " + str(q->b) + " != 0"); if (q->t == POW && !hasX(q->b)) { double k = ev(q->b, 0); if (k < 0 && fabs(k - round(k)) < 1e-12) res.push_back("Potencia negativa: " + str(q->a) + " != 0"); }
        if (q->t == FN && q->f == "sqrt") res.push_back("Raiz: " + str(q->a) + " >= 0"); if (q->t == FN && q->f == "log") res.push_back("Logaritmo: " + str(q->a) + " > 0");
        if (q->t == FN && q->f == "tan") res.push_back("Tangente: cos(" + str(q->a) + ") != 0"); if (q->t == FN && (q->f == "asin")) res.push_back("Arcoseno: -1 <= " + str(q->a) + " <= 1"); });
    const double A = -200, Bn = 200; const int n = 8000; double h = (Bn - A) / n, lo = INFINITY, hi = -INFINITY; int il = 0, ih = 0; vector<char> ok(n + 1); Fn F = mkf(f); bool any = false;
    for (int i = 0; i <= n; i++) { double y = F(A + i * h); ok[i] = isfinite(y); if (ok[i]) { any = true; if (y < lo) { lo = y; il = i; } if (y > hi) { hi = y; ih = i; } } }
    if (!any) fail("La funcion no toma valores reales en [-200, 200]; revise la expresion.");
    auto edge = [&](double v, double iv) { double a = v, b = iv; for (int k = 0; k < 50; k++) { double m = (a + b) / 2; if (isfinite(F(m))) a = m; else b = m; } return cl(a); };
    vector<string> parts; int s = -1;
    for (int i = 0; i <= n; i++) {
        if (ok[i] && s < 0) s = i;
        if ((!ok[i] || i == n) && s >= 0) { int e = ok[i] ? i : i - 1; string l, r;
            if (s == 0) l = "(-inf"; else { double x = edge(A + s * h, A + (s - 1) * h); l = string(isfinite(F(x)) ? "[" : "(") + fnum(x); }
            if (e == n) r = "+inf)"; else { double x = edge(A + e * h, A + (e + 1) * h); r = fnum(x) + (isfinite(F(x)) ? "]" : ")"); }
            parts.push_back(l + ", " + r); s = -1; }
    }
    string dom; if (parts.size() == 1 && parts[0] == "(-inf, +inf)") dom = "R = (-inf, +inf)"; else for (size_t i = 0; i < parts.size(); i++) dom += (i ? " U " : "") + parts[i];
    auto inf_ = [&](double v, int i) { return fabs(v) > 1e6 || ((i <= 1 || i >= n - 1) && fabs(v) > 1e3); };
    string rg = string(inf_(lo, il) ? "(-inf" : "[" + fnum(lo)) + ", " + (inf_(hi, ih) ? "+inf)" : fnum(hi) + "]");
    string rs; for (auto& r : res) rs += r + "; ";
    rep("Dominio: valores de x donde f(x) es real y finita. Rango: valores que toma f(x).", "f(x) = " + str(f),
        {res.empty() ? "Sin restricciones algebraicas (no hay denominadores, raices ni logaritmos)." : "Restricciones: " + rs, "Se evalua f en 8001 puntos de [-200, 200] para ubicar los tramos donde esta definida.", "El rango se estima con el minimo y el maximo hallados y el comportamiento en los extremos."},
        "Dominio: " + dom + "   |   Rango aprox.: " + rg, {"El rango es una estimacion numerica en [-200, 200]; confirmelo con la grafica."});
}
void compose(const string& fs, const string& gs) {
    P f = parse(fs), g = parse(gs); P fg = simp(sub(f, g)), gf = simp(sub(g, f)); string chk = "No se hallo un punto de prueba comun.";
    for (double t : {2.0, 1.5, 0.5}) { double u = ev(f, ev(g, t)); if (isfinite(u)) { chk = "Comprobacion en x = " + fnum(t) + ": f(g(" + fnum(t) + ")) = " + fnum(u) + " y (f o g)(" + fnum(t) + ") = " + fnum(ev(fg, t)); break; } }
    rep("(f o g)(x) = f(g(x))   y   (g o f)(x) = g(f(x))", "f(x) = " + str(f) + ";  g(x) = " + str(g),
        {"f o g: reemplazar cada x de f por g(x) -> " + str(sub(f, g)), "g o f: reemplazar cada x de g por f(x) -> " + str(sub(g, f)), "Simplificar.", "Dominio de f o g: x en dom(g) tal que g(x) en dom(f).", chk},
        "(f o g)(x) = " + str(fg) + "   |   (g o f)(x) = " + str(gf));
}

// ================= MODULO: GRAFICAS =================
void graph(const string& fs, double a, double b) {
    if (a >= b) fail("El limite izquierdo debe ser menor que el derecho."); P f = parse(fs); Fn F = mkf(f); An an = analyze(f, a, b);
    vector<double> ys; for (int i = 0; i <= 800; i++) { double y = F(a + (b - a) * i / 800); if (isfinite(y)) ys.push_back(y); }
    if (ys.empty()) fail("La funcion no toma valores reales en el intervalo elegido."); sort(ys.begin(), ys.end());
    double y0 = ys[(size_t)(ys.size() * 0.02)], y1 = ys[(size_t)(ys.size() * 0.98)];
    for (auto& q : an.pts) { if (q.y > y1 && q.y < y1 + (y1 - y0)) y1 = q.y; if (q.y < y0 && q.y > y0 - (y1 - y0)) y0 = q.y; }
    if (y1 - y0 < 1e-9) { y0 -= 1; y1 += 1; } double pad = (y1 - y0) * 0.1; y0 -= pad; y1 += pad;
    const int W = 76, H = 26; vector<string> gr(H, string(W, ' '));
    auto col = [&](double x) { return (int)lround((x - a) / (b - a) * (W - 1)); }; auto row = [&](double y) { return (int)lround((y1 - y) / (y1 - y0) * (H - 1)); };
    if (y0 < 0 && y1 > 0) for (int c = 0; c < W; c++) gr[row(0)][c] = '-'; if (a < 0 && b > 0) for (int r = 0; r < H; r++) gr[r][col(0)] = '|'; if (y0 < 0 && y1 > 0 && a < 0 && b > 0) gr[row(0)][col(0)] = '+';
    int pr = -1; for (int c = 0; c < W; c++) { double y = F(a + (b - a) * c / (W - 1)); if (!isfinite(y)) { pr = -1; continue; } int r = row(y);
        if (r >= 0 && r < H) { if (pr >= 0 && abs(r - pr) < H / 2) for (int k = min(r, pr); k <= max(r, pr); k++) gr[k][c] = '*'; gr[r][c] = '*'; pr = r; } else pr = -1; }
    auto mark = [&](double x, double y, char m) { int r = row(y), c = col(x); if (r >= 0 && r < H && c >= 0 && c < W) gr[r][c] = m; };
    for (double x : an.rs) mark(x, 0, 'o'); if (a <= 0 && b >= 0 && isfinite(an.yi)) mark(0, an.yi, '@'); for (auto& q : an.pts) mark(q.x, q.y, '#');
    cout << "\n   y max = " << fnum(y1) << "\n"; for (auto& r : gr) cout << "   |" << r << "\n";
    cout << "   y min = " << fnum(y0) << "     x: [" << fnum(a) << ", " << fnum(b) << "]\n   Leyenda: * curva  o interseccion con eje x  @ interseccion con eje y  # punto critico\n";
    // SVG
    ofstream o("grafica.svg"); if (o) { const int SW = 800, SH = 500; auto X = [&](double x) { return (x - a) / (b - a) * SW; }; auto Y = [&](double y) { return SH - (y - y0) / (y1 - y0) * SH; };
        o << "<svg xmlns='http://www.w3.org/2000/svg' width='" << SW << "' height='" << SH << "' style='background:#fff'>";
        if (a < 0 && b > 0) o << "<line x1='" << X(0) << "' y1='0' x2='" << X(0) << "' y2='" << SH << "' stroke='#333'/>"; if (y0 < 0 && y1 > 0) o << "<line x1='0' y1='" << Y(0) << "' x2='" << SW << "' y2='" << Y(0) << "' stroke='#333'/>";
        o << "<polyline fill='none' stroke='#1f3fa8' stroke-width='2.5' points='"; for (int i = 0; i <= SW; i++) { double y = F(a + (b - a) * i / SW); if (isfinite(y) && y > y0 - 5 * (y1 - y0) && y < y1 + 5 * (y1 - y0)) o << i << "," << Y(y) << " "; } o << "'/>";
        for (double x : an.rs) o << "<circle cx='" << X(x) << "' cy='" << Y(0) << "' r='5' fill='#e11d48'/>"; for (auto& q : an.pts) o << "<circle cx='" << X(q.x) << "' cy='" << Y(q.y) << "' r='5' fill='#f59e0b'/>"; o << "</svg>"; cout << "   (Se guardo tambien la grafica en el archivo grafica.svg)\n"; }
    string r1, r2; for (double x : an.rs) r1 += "(" + fnum(cl(x)) + ", 0) "; for (auto& q : an.pts) r2 += q.t + " en (" + fnum(q.x) + ", " + fnum(q.y) + "); ";
    string iv; for (auto& s : an.iv) iv += s + "   ";
    bool par = true, imp = true; for (double x : {-1.7, 0.8, 2.3}) { double u = F(x), v = F(-x); if (!(fabs(u - v) < 1e-9 * (1 + fabs(u)))) par = false; if (!(fabs(u + v) < 1e-9 * (1 + fabs(u)))) imp = false; }
    rep("Intersecciones: f(x)=0 (eje x) y f(0) (eje y). Crecimiento: signo de f'(x). Extremos: f'(x)=0.", "f(x) = " + str(f) + " en [" + fnum(a) + ", " + fnum(b) + "]",
        {"Intersecciones con el eje x: " + (r1.empty() ? string("ninguna en el intervalo") : r1), "Interseccion con el eje y: " + (a <= 0 && b >= 0 && isfinite(an.yi) ? "(0, " + fnum(an.yi) + ")" : string("fuera de la ventana o no definida")), "f'(x) = " + str(an.d1) + "  -> puntos importantes: " + (r2.empty() ? string("ninguno") : r2), "Crecimiento/decrecimiento: " + iv, string("Simetria: ") + (par ? "par (respecto al eje y)" : imp ? "impar (respecto al origen)" : "ni par ni impar")},
        "Ver la grafica y la interpretacion del procedimiento.");
}

// ================= MODULO: LIMITES =================
string nice(double v) {
    if (!isfinite(v)) return fnum(v); double r = round(v); if (fabs(v - r) < 1e-4) return fnum(r);
    for (int d = 2; d <= 12; d++) { double nn = round(v * d); if (fabs(v - nn / d) < 2e-5) return fnum(nn) + "/" + to_string(d) + " = " + fnum(v); } return fnum(round(v * 1e5) / 1e5);
}
struct Side { int kind = 0; double v = 0; int sg = 1; };  // 0 valor,1 inf,2 nan,3 osc
Side probe(const Fn& F, const function<double(int)>& X) {
    double v[3]; int ks[3] = {4, 6, 8}; Side s; for (int i = 0; i < 3; i++) { v[i] = F(X(ks[i])); if (isnan(v[i])) { s.kind = 2; return s; } }
    if (!isfinite(v[2]) || (fabs(v[2]) > 1e5 && fabs(v[2]) > 2 * fabs(v[1]))) { s.kind = 1; s.sg = v[2] < 0 ? -1 : 1; return s; }
    double d1 = v[1] - v[0], d2 = v[2] - v[1];
    if (fabs(d2) < 1e-4 * (1 + fabs(v[2]))) { s.kind = 0; s.v = v[2]; return s; }
    if (fabs(d2) < 0.25 * fabs(d1) && fabs(d2) < 5e-3) { double r = d2 / d1; s.kind = 0; s.v = v[2] + d2 * r / (1 - r); return s; }  // convergencia lenta (p. ej. raiz)
    s.kind = 3; return s;
}
string lab(const Side& s) { return s.kind == 0 ? nice(s.v) : s.kind == 1 ? (s.sg > 0 ? "+inf" : "-inf") : s.kind == 2 ? "no definido" : "no converge (oscila)"; }
bool sameS(const Side& u, const Side& v) { return (u.kind == 0 && v.kind == 0 && fabs(u.v - v.v) < 1e-4 * (1 + fabs(u.v))) || (u.kind == 1 && v.kind == 1 && u.sg == v.sg); }
void limit(const string& fs, string as, char side) {
    P f = parse(fs); Fn F = mkf(f); for (auto& c : as) c = tolower(c); as.erase(remove(as.begin(), as.end(), ' '), as.end());
    if (as.empty()) fail("Indique el valor al que tiende x (numero, pi, inf o -inf).");
    int inf = 0; double a = 0; if (as == "inf" || as == "+inf" || as == "infinito") inf = 1; else if (as == "-inf" || as == "-infinito") inf = -1; else a = numv(as);
    Side L = probe(F, [&](int k) { return inf ? inf * pow(10.0, k) : a - pow(10.0, -k); }), Rt = inf ? L : probe(F, [&](int k) { return a + pow(10.0, -k); });
    string ans; if (inf || side == 'l') ans = "lim = " + lab(L); else if (side == 'r') ans = "lim = " + lab(Rt); else if (sameS(L, Rt)) ans = "lim = " + lab(L);
    else ans = "El limite bilateral NO existe. Lateral izquierdo = " + lab(L) + "; lateral derecho = " + lab(Rt);
    vector<string> pr; P t = f; if (!inf) {
        double dv = F(a); pr.push_back("Sustitucion directa: f(" + fnum(a) + ") = " + (isfinite(dv) ? fnum(dv) : string("indefinido")) + ".");
        if (isfinite(dv) && L.kind == 0 && fabs(dv - L.v) < 1e-3) pr.push_back("f es continua en a: el limite es el valor de la sustitucion.");
        else if (t->t == DIV) { Fn Nn = mkf(t->a), Dn = mkf(t->b);
            if (fabs(Nn(a)) < 1e-9 && fabs(Dn(a)) < 1e-9) { pr.push_back("Resulta 0/0 (forma indeterminada): factorizar y simplificar, racionalizar o aplicar L'Hopital."); double q = ev(der(t->a), a) / ev(der(t->b), a); if (isfinite(q)) pr.push_back("Verificacion con L'Hopital: N'(a)/D'(a) = " + nice(q) + "."); }
            else if (fabs(Dn(a)) < 1e-9) pr.push_back("El denominador tiende a 0 y el numerador no: hay asintota vertical (el limite es +-inf o no existe por lados)."); }
    } else { pr.push_back("Limite al infinito: se divide entre la mayor potencia de x (o se comparan grados) y se estudia f(x) para |x| muy grande."); if (t->t == DIV) pr.push_back("Cociente: grados iguales -> razon de coeficientes principales; grado del numerador menor -> 0; mayor -> +-inf."); }
    string tab = "Tabla de valores:"; for (int k = 1; k <= 4; k++) { if (inf) { double x = inf * pow(10.0, k); tab += "  f(" + fnum(x) + ")=" + fnum(F(x)); } else { double x1 = a - pow(10.0, -k), x2 = a + pow(10.0, -k); tab += "  f(" + fnum(x1) + ")=" + fnum(F(x1)) + "  f(" + fnum(x2) + ")=" + fnum(F(x2)); } }
    pr.push_back(tab); pr.push_back("Por la izquierda -> " + lab(L) + ";  por la derecha -> " + lab(Rt) + ".");
    rep("lim(x->a) f(x) = L si f(x) se acerca a L cuando x se acerca a a. El limite bilateral existe solo si ambos laterales coinciden.", "f(x) = " + str(f) + ",  x -> " + (inf ? (inf > 0 ? string("+inf") : string("-inf")) : fnum(a)), pr, ans, {"Resultado por aproximacion numerica con verificacion de la forma algebraica; confirme con el procedimiento exacto."});
}

// ================= MODULO: DERIVADAS =================
void derive(const string& fs, int ord, const string& at) {
    P f = parse(fs); vector<P> ds = {f}; vector<string> pr = rules(f);
    for (int i = 0; i < ord; i++) ds.push_back(der(ds.back()));
    const char* nm[] = {"f'(x)", "f''(x)", "f'''(x)"}; string ans; vector<string> wr; int cnt = 0; bool ok = true;
    for (int i = 1; i <= ord; i++) { pr.push_back(string("Aplicando y simplificando: ") + nm[i - 1] + " = " + str(ds[i])); ans += string(i > 1 ? "   |   " : "") + nm[i - 1] + " = " + str(ds[i]);
        for (double x : {0.5, 1.5, -1.2, 2.3}) { double u = ev(ds[i], x), v = dnum(mkf(ds[i - 1]), x); if (isfinite(u) && isfinite(v)) { cnt++; if (fabs(u - v) > 1e-3 * (1 + fabs(u))) ok = false; } } }
    pr.push_back(cnt ? (ok ? "Verificacion: la derivada simbolica coincide con la diferencia central [f(x+h)-f(x-h)]/2h en " + to_string(cnt) + " puntos." : "La verificacion numerica no coincide en algunos puntos.") : "No se pudo verificar numericamente (f no definida en los puntos de prueba).");
    if (!ok) wr.push_back("Revise posibles discontinuidades."); if (!at.empty()) { double a = numv(at); ans += string("   |   ") + nm[ord - 1] + " en x = " + fnum(a) + ": " + fnum(ev(ds[ord], a)); }
    rep("f'(x) = lim(h->0) [f(x+h) - f(x)]/h, aplicado mediante las reglas de derivacion.", "f(x) = " + str(f), pr, ans, wr);
}

// ================= MODULO: INTEGRALES =================
P integ(const P& n);
bool lin(const P& u, double& al, double& be) { Pol c; if (!poly(u, c) || c.size() != 2) return false; al = c[1]; be = c[0]; return true; }
P integ(const P& n) {
    if (!hasX(n)) return mk(MUL, n, var());
    Pol c; if (poly(n, c)) { P r = nullptr; for (size_t k = 0; k < c.size(); k++) { if (fabs(c[k]) < 1e-14) continue; P t = mk(MUL, num(c[k] / (k + 1)), mk(POW, var(), num(k + 1))); r = r ? mk(ADD, r, t) : t; } return r ? r : num(0); }
    double al, be;
    switch (n->t) {
    case NEG: { P r = integ(n->a); return r ? mk(NEG, r) : nullptr; }
    case ADD: case SUB: { P u = integ(n->a), v = integ(n->b); return u && v ? mk(n->t, u, v) : nullptr; }
    case MUL: {
        if (!hasX(n->a)) { P r = integ(n->b); return r ? mk(MUL, n->a, r) : nullptr; } if (!hasX(n->b)) { P r = integ(n->a); return r ? mk(MUL, n->b, r) : nullptr; }
        for (int k = 0; k < 2; k++) { P p = k ? n->b : n->a, g = k ? n->a : n->b; Pol pc; if (poly(p, pc) && g->t == FN && (g->f == "sin" || g->f == "cos" || g->f == "exp") && lin(g->a, al, be)) {
            P G = integ(g); if (!G) return nullptr; P dp = der(p); P h = integ(simp(mk(MUL, dp, G))); return h ? mk(SUB, mk(MUL, p, G), h) : nullptr; } }
        return nullptr; }
    case DIV: {
        if (!hasX(n->b)) { P r = integ(n->a); return r ? mk(DIV, r, n->b) : nullptr; }
        if (!hasX(n->a) && lin(n->b, al, be)) return mk(MUL, mk(DIV, n->a, num(al)), fn("log", fn("abs", n->b)));
        if (!hasX(n->a) && n->b->t == POW && !hasX(n->b->b) && lin(n->b->a, al, be)) { double k = -ev(n->b->b, 0); P q = mk(POW, n->b->a, num(1 - k)); if (fabs(k - 1) < 1e-12) return mk(MUL, mk(DIV, n->a, num(al)), fn("log", fn("abs", n->b->a))); return mk(DIV, mk(MUL, n->a, q), num((1 - k) * al)); }
        return nullptr; }
    case POW:
        if (!hasX(n->b) && lin(n->a, al, be)) { double k = ev(n->b, 0); if (fabs(k + 1) < 1e-12) return mk(DIV, fn("log", fn("abs", n->a)), num(al)); return mk(DIV, mk(POW, n->a, num(k + 1)), num((k + 1) * al)); }
        if (!hasX(n->a) && ev(n->a, 0) > 0 && lin(n->b, al, be)) return mk(DIV, n, num(al * log(ev(n->a, 0)))); return nullptr;
    case FN:
        if (!lin(n->a, al, be)) return nullptr; { const string& f = n->f; P u = n->a;
        if (f == "sin") return mk(DIV, mk(NEG, fn("cos", u)), num(al)); if (f == "cos") return mk(DIV, fn("sin", u), num(al)); if (f == "exp") return mk(DIV, n, num(al));
        if (f == "tan") return mk(DIV, mk(NEG, fn("log", fn("abs", fn("cos", u)))), num(al)); if (f == "sqrt") return mk(DIV, mk(MUL, num(2.0 / 3), mk(POW, u, num(1.5))), num(al));
        if (f == "log") return mk(DIV, mk(SUB, mk(MUL, u, n), u), num(al)); if (f == "atan") return mk(DIV, mk(SUB, mk(MUL, u, n), mk(DIV, fn("log", mk(ADD, num(1), mk(POW, u, num(2)))), num(2))), num(al)); }
        return nullptr;
    default: return nullptr;
    }
}
void integrate(const string& fs, const string& A, const string& B) {
    if (A.empty() != B.empty()) fail("Para la integral definida complete a y b; para la indefinida deje ambos vacios."); P f = parse(fs); Fn F = mkf(f);
    P Fx = nullptr; try { Fx = integ(f); } catch (...) {} if (Fx) Fx = simp(simp(Fx));
    vector<string> rl = {"Linealidad: Int(k*f +- g) = k*Int f +- Int g.", "Potencia: Int x^n dx = x^(n+1)/(n+1) + C (n != -1).", "Int 1/x dx = ln|x| + C;  Int e^x dx = e^x + C;  Int sen x dx = -cos x + C;  Int cos x dx = sen x + C.", "Si hay producto polinomio*funcion: integracion por partes Int u dv = uv - Int v du."};
    string ver; if (Fx) { Fn G = mkf(Fx); int q = 0; bool ok = true; for (double x : {0.7, 1.3, 2.1, -0.9}) { double u = G(x), v = F(x); if (isfinite(u) && isfinite(v)) { q++; if (fabs(dnum(G, x) - v) > 1e-3 * (1 + fabs(v))) ok = false; } } ver = q ? (ok ? "Verificacion: F'(x) = f(x) en los puntos de prueba." : "AVISO: la verificacion F' = f no coincide en algun punto.") : "Derive el resultado para comprobar."; }
    if (A.empty()) { if (!Fx) fail("No se encontro una antiderivada con las reglas implementadas (potencias, sumas, sen, cos, exp, ln, polinomio*trigonometrica/exp). Pruebe con una expresion mas simple o use limites para una aproximacion numerica."); rep("Int f(x) dx = F(x) + C, donde F'(x) = f(x)", "Int (" + str(f) + ") dx", [&] { auto v = rl; v.push_back(ver); return v; }(), str(Fx) + " + C"); return; }
    double a = numv(A), b = numv(B); vector<string> w; bool bad = false; for (int i = 0; i < 60; i++) if (!isfinite(F(a + (b - a) * (i + .5) / 60))) bad = true;
    if (bad) w.push_back("La funcion no esta definida o no es acotada en parte de [a, b]: podria ser impropia; el resultado no es confiable.");
    double s2 = simpson(F, a, b), v1 = Fx ? mkf(Fx)(b) - mkf(Fx)(a) : NAN, v = isfinite(v1) ? v1 : s2;
    if (isfinite(v1) && isfinite(s2) && fabs(v1 - s2) > 1e-3 * (1 + fabs(v1))) w.push_back("El valor exacto y la aproximacion numerica difieren; revise discontinuidades.");
    vector<string> pr = Fx ? rl : vector<string>{"No se hallo antiderivada con las reglas implementadas: se usa la regla de Simpson."};
    if (Fx) pr.push_back("F(b) - F(a) = " + fnum(mkf(Fx)(b)) + " - " + fnum(mkf(Fx)(a)) + " = " + fnum(v1)); pr.push_back("Comprobacion numerica (Simpson, n = 2000): " + fnum(s2));
    rep("Teorema fundamental: Int[a,b] f(x) dx = F(b) - F(a)", "Int de " + fnum(a) + " a " + fnum(b) + " de " + str(f) + " dx" + (Fx ? ";  F(x) = " + str(Fx) : ""), pr, "Integral = " + fnum(v), w);
}

// ================= MODULO: APLICACIONES =================
void extremos(const string& fs, bool hasI, double a, double b, const string& pre = "") {
    P f = parse(fs); if (!hasI) { a = -50; b = 50; } if (a >= b) fail("Debe cumplirse a < b."); An an = analyze(f, a, b); Fn F = mkf(f);
    string tb = "Tabla de puntos criticos:"; for (auto& q : an.pts) tb += "  [x=" + fnum(q.x) + ", f=" + fnum(q.y) + ", f''=" + fnum(q.s) + " -> " + q.t + "]";
    string ans; for (auto& q : an.pts) ans += q.t + ": (" + fnum(q.x) + ", " + fnum(q.y) + ");  "; if (ans.empty()) ans = "No hay puntos criticos en el intervalo analizado.";
    vector<string> w; if (hasI) { vector<pair<double, double>> cand = {{a, F(a)}, {b, F(b)}}; for (auto& q : an.pts) cand.push_back({q.x, q.y}); double mx = -INFINITY, mn = INFINITY, xm = 0, xn = 0; for (auto& c : cand) if (isfinite(c.second)) { if (c.second > mx) { mx = c.second; xm = c.first; } if (c.second < mn) { mn = c.second; xn = c.first; } }
        ans += "\n >> En [" + fnum(a) + ", " + fnum(b) + "]: maximo absoluto f(" + fnum(xm) + ") = " + fnum(mx) + ";  minimo absoluto f(" + fnum(xn) + ") = " + fnum(mn); } else w.push_back("Sin intervalo se busco en [-50, 50]. Indique [a, b] para extremos absolutos.");
    string iv; for (auto& s : an.iv) iv += s + "   ";
    rep("Puntos criticos: f'(x) = 0. Criterio de la 2a derivada: f''>0 minimo, f''<0 maximo; si f''=0 se estudia el signo de f'.", pre + "f(x) = " + str(f),
        {"Derivar: f'(x) = " + str(an.d1), string("Resolver f'(x) = 0 -> x = ") + [&] { string s; for (auto& q : an.pts) s += fnum(q.x) + " "; return s.empty() ? string("ninguno") : s; }(), "Segunda derivada: f''(x) = " + str(an.d2), tb, "Signo de f' por intervalos: " + iv}, ans, w);
}
void rate(const string& fs, double a, const string& bs, const string& u) {
    P f = parse(fs); double fa = ev(f, a); if (!isfinite(fa)) fail("f no esta definida en x = " + fnum(a) + "."); P d1 = der(f); double ga = ev(d1, a);
    vector<string> pr = {"f'(x) = " + str(d1), "Razon instantanea en x = " + fnum(a) + ": f'(" + fnum(a) + ") = " + fnum(ga) + " " + u}; string ans = "f'(" + fnum(a) + ") = " + fnum(ga) + " " + u;
    if (!bs.empty()) { double b = numv(bs); if (b == a) fail("Los puntos a y b deben ser distintos."); double m = (ev(f, b) - fa) / (b - a); pr.push_back("Razon promedio en [" + fnum(a) + ", " + fnum(b) + "] = [f(b)-f(a)]/(b-a) = (" + fnum(ev(f, b)) + " - " + fnum(fa) + ")/" + fnum(b - a) + " = " + fnum(m)); ans += "   |   Razon promedio = " + fnum(m) + " " + u; }
    pr.push_back(ga > 0 ? "f' > 0: la cantidad aumenta en ese instante." : ga < 0 ? "f' < 0: la cantidad disminuye en ese instante." : "f' = 0: cambio instantaneo nulo (posible extremo).");
    rep("Razon promedio = delta f / delta x.  Razon instantanea = f'(a).", "f(x) = " + str(f) + ";  a = " + fnum(a), pr, ans);
}
void optim(int k, double p1, double p2) {
    if (p1 <= 0 || p2 < 0) fail("Los datos deben ser positivos."); string fs, m; double a = 0, b = 0;
    if (k == 1) { fs = "x*(" + fnum(p1) + "/2-x)"; b = p1 / 2; m = "x = base, altura = P/2 - x; A(x) = x(P/2 - x); 0 <= x <= P/2. "; }
    else if (k == 2) { fs = "x*(" + fnum(p1) + "-2*x)^2"; b = p1 / 2; m = "x = lado del cuadrado recortado; V(x) = x(L - 2x)^2; 0 <= x <= L/2. "; }
    else if (k == 3) { fs = "x*(" + fnum(p1) + "-2*x)"; b = p1 / 2; m = "x = lado perpendicular al rio (2 lados), paralelo = F - 2x; A(x) = x(F - 2x); 0 <= x <= F/2. "; }
    else { fs = "-4.9*x^2+" + fnum(p1) + "*x+" + fnum(p2); b = (p1 + sqrt(p1 * p1 + 19.6 * p2)) / 9.8; m = "t = tiempo (s); h(t) = -4.9t^2 + v0 t + h0; 0 <= t <= tiempo de impacto. "; }
    extremos(fs, true, a, b, "Modelo: " + m + "\n");
}

// ================= PRACTICA =================
mt19937 rng(random_device{}());
int ri(int a, int b) { return uniform_int_distribution<int>(a, b)(rng); } int nz(int a, int b) { int v = 0; while (!v) v = ri(a, b); return v; }
string sg(double v) { return v < 0 ? "-" + fnum(-v) : "+" + fnum(v); }
struct Ex { string q; function<bool(const string&)> chk; function<void()> sol; };
Ex genEx(int t) {
    Ex e;
    if (t == 1) { int a = nz(-5, 5), b = nz(-6, 6), k = ri(1, 4), m = ri(1, 5); vector<string> fs = {to_string(a) + "*x^3" + sg(b) + "*x^2" + sg(ri(-8, 8)) + "*x", "x^2*sin(x)", "(x^2+" + to_string(m) + ")/(x-" + to_string(k) + ")", to_string(nz(1, 4)) + "*exp(" + to_string(ri(2, 3)) + "*x)+" + to_string(nz(2, 5)) + "*log(x)"}; string f = fs[ri(0, 3)];
        e.q = "Calcule f'(x) para f(x) = " + f; e.sol = [f] { derive(f, 1, ""); };
        e.chk = [f](const string& s) { P u = parse(s), dd = der(parse(f)); int c = 0; for (double x : {0.37, 1.13, 2.71, 4.9}) { double p = ev(u, x), q = ev(dd, x); if (isfinite(p) && isfinite(q)) { c++; if (fabs(p - q) > 1e-6 * (1 + fabs(q))) return false; } } return c >= 2; }; }
    else if (t == 2) { int a = nz(-5, 5), b = nz(-6, 6); string f = ri(0, 1) ? to_string(a) + "*x^2" + sg(b) + "*x" + sg(ri(-5, 5)) : to_string(a) + "*cos(x)" + sg(b) + "*x";
        e.q = "Calcule la integral indefinida de (" + f + ") dx (escriba la antiderivada, sin +C)"; e.sol = [f] { integrate(f, "", ""); };
        e.chk = [f](const string& s) { P u = parse(s), g = parse(f); int c = 0; for (double x : {0.5, 1.1, 2.3, -0.8}) { double p = ev(u, x); if (isfinite(p)) { c++; if (fabs(dnum(mkf(u), x) - ev(g, x)) > 1e-3 * (1 + fabs(ev(g, x)))) return false; } } return c >= 2; }; }
    else if (t == 3) { int k = ri(0, 2); string f, a; double v; if (k == 0) { int q = nz(-5, 5); f = "(x^2-" + to_string(q * q) + ")/(x-" + to_string(q) + ")"; if (q < 0) f = "(x^2-" + to_string(q * q) + ")/(x+" + to_string(-q) + ")"; a = to_string(q); v = 2 * q; }
        else if (k == 1) { int A = nz(-6, 6), c = nz(1, 5); f = "(" + to_string(A) + "*x^2+" + to_string(ri(1, 9)) + ")/(" + to_string(c) + "*x^2+" + to_string(ri(1, 9)) + ")"; a = "inf"; v = (double)A / c; }
        else { int m = nz(2, 7); f = "sin(" + to_string(m) + "*x)/x"; a = "0"; v = m; }
        e.q = "Calcule el limite cuando x -> " + a + " de " + f + "   (puede escribir fracciones: 3/2)"; e.sol = [f, a] { limit(f, a, 'b'); }; e.chk = [v](const string& s) { return fabs(numv(s) - v) < 1e-3; }; }
    else if (t == 4) { int r1 = nz(-6, 6), r2 = nz(-6, 6); string f = "x^2" + sg(-(r1 + r2)) + "*x" + sg(r1 * r2) + "=0"; e.q = "Resuelva " + f + "  (soluciones separadas por coma)"; e.sol = [f] { solveEq(f); };
        e.chk = [r1, r2](const string& s) { auto u = listv(s); sort(u.begin(), u.end()); set<int> w = {r1, r2}; vector<double> ww(w.begin(), w.end()); if (u.size() != ww.size()) return false; for (size_t i = 0; i < u.size(); i++) if (fabs(u[i] - ww[i]) > 1e-6) return false; return true; }; }
    else if (t == 5) { int m = nz(-7, 7), n = nz(-7, 7); string f = "x^2" + sg(m + n) + "*x" + sg(m * n); e.q = "Factorice " + f; e.sol = [f] { factorExpr(f); };
        e.chk = [f](const string& s) { if (s.find('(') == string::npos) return false; P u = parse(s), g = parse(f); for (double x : {-3.3, 0.7, 2.9, 5.1}) if (fabs(ev(u, x) - ev(g, x)) > 1e-6 * (1 + fabs(ev(g, x)))) return false; return true; }; }
    else { int p = nz(-3, 3), tt = nz(-3, 3), q = p + 2 * tt, d = ri(-5, 5); string f = "x^3" + sg(-3 * (p + tt)) + "*x^2" + sg(3 * p * q) + "*x" + sg(d); e.q = "Halle los puntos criticos de f(x) = " + f + "  (valores de x separados por coma)"; e.sol = [f] { extremos(f, false, 0, 0); };
        e.chk = [p, q](const string& s) { auto u = listv(s); sort(u.begin(), u.end()); set<int> w = {p, q}; vector<double> ww(w.begin(), w.end()); if (u.size() != ww.size()) return false; for (size_t i = 0; i < u.size(); i++) if (fabs(u[i] - ww[i]) > 1e-6) return false; return true; }; }
    return e;
}

// ================= MENU =================
string ask(const string& p) { cout << p; string s; if (!getline(cin, s)) { cout << "\nHasta pronto.\n"; exit(0); } return s; }
double askn(const string& p, bool opt = false, double def = 0) { string s = ask(p); if (s.find_first_not_of(' ') == string::npos) { if (opt) return def; fail("Falta un dato numerico obligatorio."); } return numv(s); }
void practica() {
    cout << "\nTemas: 1 Derivadas  2 Integrales  3 Limites  4 Ecuaciones cuadraticas  5 Factorizacion  6 Puntos criticos\n"; int t = (int)askn("Tema: "); if (t < 1 || t > 6) fail("Elija un tema del 1 al 6.");
    int ok = 0, n = 0; for (;;) {
        Ex e = genEx(t); cout << "\nEJERCICIO: " << e.q << "\n"; bool first = true, done = false;
        while (!done) { string r = ask("Su respuesta (o 'sol' para ver la solucion, 'fin' para terminar): "); if (r == "fin") { cout << "Aciertos al primer intento: " << ok << " de " << n << ".\n"; return; }
            if (r == "sol") { e.sol(); break; } if (r.find_first_not_of(' ') == string::npos) { cout << " [AVISO] Escriba una respuesta.\n"; continue; }
            bool good; try { good = e.chk(r); } catch (UErr& x) { cout << " [AVISO] " << x.what() << "\n"; continue; }
            if (first) { n++; if (good) ok++; first = false; }
            if (good) { cout << " CORRECTO! Compare su procedimiento con la solucion modelo:\n"; e.sol(); done = true; } else cout << " Todavia no coincide. Revise signos y reglas; intente de nuevo o escriba 'sol'.\n"; }
        if (ask("Otro ejercicio? (s/n): ") != "s") { cout << "Aciertos al primer intento: " << ok << " de " << n << ".\n"; return; }
    }
}
struct Demo { string name; function<void()> run; };
vector<Demo> demos() { return {
    {"Ecuacion cuadratica x^2-5x+6=0", [] { solveEq("x^2-5x+6=0"); }}, {"Ecuacion racional (x+1)/(x-2)=3", [] { solveEq("(x+1)/(x-2)=3"); }},
    {"Factorizar x^3-x^2-4x+4", [] { factorExpr("x^3-x^2-4x+4"); }}, {"Dominio y rango de sqrt(4-x^2)", [] { domRange("sqrt(4-x^2)"); }},
    {"Composicion f=x^2+1, g=sqrt(x)", [] { compose("x^2+1", "sqrt(x)"); }}, {"Grafica x^3-6x^2+9x+1", [] { graph("x^3-6x^2+9x+1", -2, 5); }},
    {"Limite (x^2-9)/(x-3), x->3", [] { limit("(x^2-9)/(x-3)", "3", 'b'); }}, {"Limite (3x^2+1)/(x^2-5), x->inf", [] { limit("(3x^2+1)/(x^2-5)", "inf", 'b'); }},
    {"Limite sin(x)/x, x->0", [] { limit("sin(x)/x", "0", 'b'); }}, {"Derivada de x^2*sin(x) en x=1", [] { derive("x^2*sin(x)", 1, "1"); }},
    {"Derivada segunda de (x^2+1)/(x-3)", [] { derive("(x^2+1)/(x-3)", 2, ""); }}, {"Integral indefinida 3x^2+2x+1", [] { integrate("3x^2+2x+1", "", ""); }},
    {"Integral definida sin(x) de 0 a pi", [] { integrate("sin(x)", "0", "pi"); }}, {"Extremos de f(x)=x^3-6x^2+9x+1 (EJEMPLO DE PRUEBA)", [] { extremos("x^3-6x^2+9x+1", false, 0, 0); }},
    {"Razon de cambio de x^3 en x=2 (promedio hasta 4)", [] { rate("x^3", 2, "4", "unidades/s"); }}, {"Optimizacion: rectangulo de perimetro 40", [] { optim(1, 40, 0); }}}; }
int main() {
#ifdef _WIN32
    SetConsoleOutputCP(65001);
#endif
    cout << "\n==================================================\n  CalcuGuia - Asistente de Calculo y Precalculo\n  Universidad Mariano Galvez de Guatemala - Ingenieria\n  Integrantes: " << INTEGRANTES[0] << " / " << INTEGRANTES[1] << "\n  Asignatura: " << ASIGNATURA << "   " << SECCION << "\n==================================================\n";
    cout << "Sintaxis: * multiplica, ^ potencia, funciones con parentesis: sin(x) cos(x) tan(x) exp(x) log(x)=ln sqrt(x) abs(x); constantes pi, e. Variable: x.\n";
    for (;;) {
        cout << "\n--- MENU ---\n 1 Algebra y funciones   2 Graficas   3 Limites   4 Derivadas   5 Integrales\n 6 Aplicaciones          7 Practica    8 Ejemplos (16)             0 Salir\n";
        string o = ask("Opcion: ");
        try {
            if (o == "0") break;
            else if (o == "1") { cout << " a) Resolver ecuacion  b) Factorizar  c) Dominio y rango  d) Composicion\n"; string s = ask("Operacion: "); if (s == "a") solveEq(ask("Ecuacion (ej. x^2-5x+6=0): ")); else if (s == "b") factorExpr(ask("Polinomio: ")); else if (s == "c") domRange(ask("f(x): ")); else if (s == "d") { string f = ask("f(x): "); compose(f, ask("g(x): ")); } else fail("Opcion no valida."); }
            else if (o == "2") { string f = ask("f(x): "); double a = askn("x minimo [-10]: ", true, -10), b = askn("x maximo [10]: ", true, 10); graph(f, a, b); }
            else if (o == "3") { string f = ask("f(x): "), a = ask("x tiende a (numero, inf o -inf): "), s = ask("Lado (b=bilateral, l=izquierda, r=derecha) [b]: "); limit(f, a, s.empty() ? 'b' : tolower(s[0])); }
            else if (o == "4") { string f = ask("f(x): "); int n = (int)askn("Orden (1-3) [1]: ", true, 1); if (n < 1 || n > 3) fail("El orden debe ser 1, 2 o 3."); derive(f, n, ask("Evaluar en x = (opcional): ")); }
            else if (o == "5") { string f = ask("f(x): "), a = ask("Limite inferior a (vacio = indefinida): "), b = a.empty() ? "" : ask("Limite superior b: "); integrate(f, a, b); }
            else if (o == "6") { cout << " a) Maximos y minimos  b) Razon de cambio  c) Optimizacion\n"; string s = ask("Problema: "); if (s == "a") { string f = ask("f(x): "), a = ask("Intervalo a (opcional): "), b = ask("Intervalo b (opcional): "); if (a.empty() != b.empty()) fail("Complete ambos extremos o ninguno."); extremos(f, !a.empty(), a.empty() ? 0 : numv(a), b.empty() ? 0 : numv(b)); }
                else if (s == "b") { string f = ask("f(x): "); double a = askn("Punto a: "); string b = ask("Punto b (opcional): "), u = ask("Unidades (opcional): "); rate(f, a, b, u); }
                else if (s == "c") { cout << " 1 Rectangulo con perimetro P (area max)  2 Caja sin tapa de lamina cuadrada L (volumen max)\n 3 Cerca junto a un rio con F metros (area max)  4 Proyectil (altura max)\n"; int k = (int)askn("Situacion: "); if (k < 1 || k > 4) fail("Elija 1 a 4."); double p1 = askn("Dato 1 (P, L, F o v0): "), p2 = k == 4 ? askn("Altura inicial h0: ") : 0; optim(k, p1, p2); } else fail("Opcion no valida."); }
            else if (o == "7") practica();
            else if (o == "8") { auto D = demos(); for (size_t i = 0; i < D.size(); i++) cout << " " << i + 1 << ". " << D[i].name << "\n"; int k = (int)askn("Numero de ejemplo: "); if (k < 1 || k > (int)D.size()) fail("Ejemplo no valido."); D[k - 1].run(); }
            else fail("Opcion no valida. Escriba un numero del 0 al 8.");
        } catch (UErr& e) { cout << "\n [ERROR DE ENTRADA] " << e.what() << "\n"; }
        catch (exception&) { cout << "\n [ERROR] No se pudo resolver este caso. Revise los datos (use *, ^ y parentesis) o simplifique la expresion.\n"; }
    }
    cout << "Hasta pronto.\n"; return 0;
}
