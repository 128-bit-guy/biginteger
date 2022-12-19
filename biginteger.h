

#include <vector>
#include <iostream>
#include <string>
#include <compare>
#include <complex>
#include <numbers>
#include <chrono>

class BigInteger;

BigInteger operator*(BigInteger a, const BigInteger& b);
BigInteger operator+(BigInteger a, const BigInteger& b);
BigInteger operator-(BigInteger a, const BigInteger& b);
BigInteger operator/(BigInteger a, const BigInteger& b);

class BigInteger {
    static const int base = static_cast<int>(1e9);
    static const int decimalDigitsPerDigit = 9;
    std::vector<int> digits;
    bool negative;
 public:
    void multiplyByNumber(int a) {
        int carry = 0;
        for (int& digit : digits) {
            long long res = static_cast<long long>(digit) * a + carry;
            digit = static_cast<int>(res % base);
            carry = static_cast<int>(res / base);
        }
        if (carry != 0) {
            digits.push_back(carry);
        }
        while(!digits.empty() && digits.back() == 0) {
            digits.pop_back();
        }
        if(digits.empty()) {
            negative = false;
        }
    }
    BigInteger() : digits(), negative(false) {

    }
    BigInteger(int a) : digits(), negative(a < 0) {
        a = abs(a);
        while (a != 0) {
            digits.push_back(a % base);
            a /= base;
        }
    }
    explicit BigInteger(const std::string& s) : digits(), negative(s[0] == '-') {
        size_t decimalDigits = s.size() - negative;
        int p = 1;
        for (size_t i = 0; i < decimalDigits; ++i) {
            size_t digitPos = i / decimalDigitsPerDigit;
            if (digits.size() <= digitPos) {
                digits.push_back(0);
            }
            size_t decimalDigitInDigitPos = i % decimalDigitsPerDigit;
            if (decimalDigitInDigitPos == 0) {
                p = 1;
            } else {
                p *= 10;
            }
            digits[digitPos] += p * static_cast<int>(s[s.size() - i - 1] - '0');
        }
        while (!digits.empty() && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.empty()) {
            negative = false;
        }
    }
    BigInteger operator-() const {
        BigInteger result = *this;
        if (!result.digits.empty()) {
            result.negative = !result.negative;
        }
        return result;
    }
    BigInteger operator+() const {
        return *this;
    }
    BigInteger& operator++() {
        if (negative) {
            decUnsigned();
            if (digits.empty()) {
                negative = false;
            }
        } else {
            incUnsigned();
        }
        return *this;
    }
    BigInteger& operator--() {
        if (digits.empty()) {
            negative = true;
        }
        if (negative) {
            incUnsigned();
        } else {
            decUnsigned();
        }
        return *this;
    }
    std::string toString() const {
        if (digits.empty()) {
            return "0";
        } else {
            std::string res(digits.size() * decimalDigitsPerDigit, '0');
            for (size_t i = 0; i < digits.size(); ++i) {
                int cLeft = digits[i];
                size_t cj = 0;
                while (cLeft != 0) {
                    res[res.size() - decimalDigitsPerDigit * i - cj - 1] = static_cast<char>((cLeft % 10) + '0');
                    ++cj;
                    cLeft /= 10;
                }
            }
            size_t i = 0;
            for (; res.size() > i && res[i] == '0'; ++i);
            res = (negative ? "-" : "") + res.substr(i);
            return res;
        }
    }

    friend std::weak_ordering operator<=>(const BigInteger& a, const BigInteger& b);

    friend bool operator==(const BigInteger& a, const BigInteger& b);

    friend bool operator!=(const BigInteger& a, const BigInteger& b);

    double toDouble() {
        double result = 0;
        double cPow = 1;
        for (int digit : digits) {
            result += static_cast<double>(digit) * cPow;
            cPow *= base;
        }
        if (negative) {
            result *= -1;
        }
        return result;
    }

    explicit operator bool() const {
        return !digits.empty();
    }

    BigInteger& operator+=(const BigInteger& other) {
        if (negative == other.negative) {
            addUnsigned(digits, other.digits);
        } else {
            std::weak_ordering unsignedOrdering = digits.size() <=> other.digits.size();
            if (unsignedOrdering == std::weak_ordering::equivalent) {
                unsignedOrdering = lexicographical_compare_three_way(digits.rbegin(),
                                                                     digits.rend(),
                                                                     other.digits.rbegin(),
                                                                     other.digits.rend());
            }
            if (unsignedOrdering == std::weak_ordering::less) {
                std::vector<int> od = other.digits;
                subtractUnsigned(od, digits);
                digits = od;
                negative = !negative;
            } else {
                subtractUnsigned(digits, other.digits);
            }
            if (digits.empty()) {
                negative = false;
            }
        }
        return *this;
    }

    BigInteger& operator-=(const BigInteger& other) {
        return *this += (-other);
    }

    BigInteger& operator*=(const BigInteger& other) {
        negative = (negative != other.negative);
        if (digits.size() <= 5 || other.digits.size() <= 5) {
            digits = multiply_numbers_simple(digits, other.digits);
        } else {
            digits = multiply_numbers(digits, other.digits);
        }
        if (digits.empty()) {
            negative = false;
        }
        return *this;
    }

    BigInteger& operator/=(const BigInteger& other) {
        if (other.negative) {
            *this /= -other;
            this->negative = !this->negative;
            if (this->digits.empty()) {
                this->negative = false;
            }
        } else {
            BigInteger carry = 0;
            std::vector<int> res(digits.size());
            for (size_t i = digits.size() - 1; i + 1 != 0; --i) {
                BigInteger cur = carry;
                cur.multiplyByNumber(base);
                cur += digits[i];
                int L = 0, R = base;
                while (L < R) {
                    int M = (L + R) >> 1;
                    BigInteger sub = other;
                    sub.multiplyByNumber(M);
                    if (sub > cur) {
                        R = M;
                    } else {
                        L = M + 1;
                    }
                }
                res[i] = L - 1;
                BigInteger other2 = other;
                other2.multiplyByNumber(L - 1);
                carry = cur - other2;
            }
            while (!res.empty() && res.back() == 0) {
                res.pop_back();
            }
            digits = res;
        }
        return *this;
    }

    BigInteger& operator%=(const BigInteger& other) {
        BigInteger d = *this / other;
        return *this -= (d * other);
    }

 private:
    void incUnsigned() {
        for (size_t i = 0;; ++i) {
            if (digits.size() <= i) {
                digits.push_back(1);
                break;
            } else {
                ++digits[i];
                if (digits[i] < base) {
                    break;
                } else {
                    digits[i] -= base;
                }
            }
        }
    }
    void decUnsigned() {
        size_t i = 0;
        for (; digits[i] == 0; ++i);
        --digits[i];
        if (digits[i] == 0 && digits.size() - 1 == i) {
            digits.pop_back();
        }
        for (; i > 0; --i) {
            digits[i - 1] = base - 1;
        }
    }

    static void addUnsigned(std::vector<int>& cur, const std::vector<int>& other, size_t curBegin = 0) {
        int carry = 0;
        for (size_t i = 0;; ++i) {
            if (carry == 0 && cur.size() <= curBegin + i && other.size() <= i) {
                break;
            }
            if (cur.size() <= curBegin + i) {
                cur.push_back(0);
            }
            int od = other.size() <= i ? 0 : other[i];
            int su = od + cur[curBegin + i] + carry;
            cur[curBegin + i] = su % base;
            carry = su / base;
        }
    }

    static void subtractUnsigned(std::vector<int>& cur, const std::vector<int>& other) {
        int carry = 0;
        for (size_t i = 0;; ++i) {
            if (carry == 0 && other.size() <= i) {
                break;
            }
            int od = other.size() <= i ? 0 : other[i];
            int su = carry + od;
            int cd = cur[i] - su;
            if (cd >= 0) {
                cur[i] = cd;
                carry = 0;
            } else {
                cur[i] = cd + base;
                carry = 1;
            }
        }
        while (!cur.empty() && cur.back() == 0) {
            cur.pop_back();
        }
    }

    static std::complex<double> from_angle(double angle) {
        return {cos(angle), sin(angle)};
    }

    static size_t reverse(size_t x, size_t l) {
        size_t res = 0;
        for (size_t i = 0; i < l; ++i) {
            if (x & (1 << i)) {
                res |= (1 << (l - i - 1));
            }
        }
        return res;
    }

    static void fft(std::vector<std::complex<double>>& a) {
        size_t l = 0;
        size_t s = a.size();
        while (s >>= 1) {
            ++l;
        }
        for (size_t i = 0; i < a.size(); ++i) {
            size_t r = reverse(i, l);
            if (r < i) {
                swap(a[i], a[r]);
            }
        }
        std::vector<std::complex<double>> from_angles(a.size());
        for (size_t x = 0; x < l; ++x) {
            for (size_t j = 0; j < (1 << x); ++j) {
                from_angles[j] = from_angle((2 * (std::numbers::pi / static_cast<double>((1ll << (x + 1))))
                    * static_cast<double>(j)));
            }
            for (size_t i = 0; i < a.size(); i += (1 << (x + 1))) {
                size_t i1 = i + (1 << x);
                for (size_t j = 0; j < (1 << x); ++j) {
                    std::complex<double> c = from_angles[j];
                    std::complex<double> res1 = a[i + j] + c * a[i1 + j];
                    std::complex<double> res2 = a[i + j] - c * a[i1 + j];
                    a[i + j] = res1;
                    a[i1 + j] = res2;
                }
            }
        }
    }

    static void inverse_fft(std::vector<std::complex<double>>& a) {
        fft(a);
        for (std::complex<double>& x : a) {
            x /= static_cast<double>(a.size());
        }
        std::reverse(a.begin() + 1, a.end());
    }

    static std::vector<long long> multiply_polynomials(const std::vector<int>& a, const std::vector<int>& b) {
        size_t p = 1;
        while (p < a.size() * 2 || p < b.size() * 2) {
            p <<= 1;
        }
        std::vector<std::complex<double>> ac(p);
        std::vector<std::complex<double>> bc(p);
        for (size_t i = 0; i < a.size(); ++i) {
            ac[i] = a[i];
        }
        for (size_t i = 0; i < b.size(); ++i) {
            bc[i] = b[i];
        }
        fft(ac);
        fft(bc);
        for (size_t i = 0; i < p; ++i) {
            ac[i] *= bc[i];
        }
        inverse_fft(ac);
        std::vector<long long> res(p);
        for (size_t i = 0; i < p; ++i) {
            res[i] = static_cast<long long>(roundl(static_cast<long double>(ac[i].real())));
        }
        while (!res.empty() && res.back() == 0) {
            res.pop_back();
        }
        return res;
    }

    static std::vector<int> toDecimal(const std::vector<int>& a) {
        std::vector<int> at;
        for (int left : a) {
            for (size_t j = 0; j < 3; ++j) {
                at.push_back(left % 1000);
                left /= 1000;
            }
        }
        while (!at.empty() && at.back() == 0) {
            at.pop_back();
        }
        return at;
    }

    static std::vector<int> multiply_numbers(const std::vector<int>& a, const std::vector<int>& b) {
        std::vector<int> at = toDecimal(a);
        std::vector<int> bt = toDecimal(b);
        std::vector<long long> rest = multiply_polynomials(at, bt);
        for (size_t i = 0; i < rest.size(); ++i) {
            if (rest[i] >= 1000) {
                long long carry = rest[i] / 1000;
                rest[i] %= 1000;
                if (i == rest.size() - 1) {
                    rest.push_back(carry);
                } else {
                    rest[i + 1] += carry;
                }
            }
        }
        std::vector<int> res(rest.size() / 3);
        int p = 1;
        for (size_t i = 0; i < rest.size(); ++i) {
            size_t digitPos = i / 3;
            if (res.size() <= digitPos) {
                res.push_back(0);
            }
            size_t decimalDigitInDigitPos = i % 3;
            if (decimalDigitInDigitPos == 0) {
                p = 1;
            } else {
                p *= 1000;
            }
            res[digitPos] += p * static_cast<int>(rest[i]);
        }
        return res;
    }
    static std::vector<int> multiply_numbers_simple(const std::vector<int>& a, const std::vector<int>& b) {
        std::vector<int> result(a.size() + b.size() + 2);
        for (size_t i = 0; i < b.size(); ++i) {
            std::vector<int> a2 = a;
            int carry = 0;
            for (size_t j = 0; j < a2.size(); ++j) {
                long long mulRes = static_cast<long long>(a2[j]) * b[i] + carry;
                a2[j] = static_cast<int>(mulRes % base);
                carry = static_cast<int>(mulRes / base);
            }
            if (carry != 0) {
                a2.push_back(carry);
            }
            addUnsigned(result, a2, i);
        }
        while (!result.empty() && result.back() == 0) {
            result.pop_back();
        }
        return result;
    }
    static std::weak_ordering lexicographical_compare_three_way(std::vector<int>::const_reverse_iterator l1,
                                                                std::vector<int>::const_reverse_iterator r1,
                                                                std::vector<int>::const_reverse_iterator l2,
                                                                std::vector<int>::const_reverse_iterator r2) {
        while (l1 != r1 && l2 != r2 && *l1 == *l2) {
            ++l1;
            ++l2;
        }
        if (l1 == r1 || l2 == r2) {
            return (l1 == r1) <=> (l2 == r2);
        } else {
            return *l1 <=> *l2;
        }
    }
};

BigInteger operator++(BigInteger& a, int) {
    BigInteger prev = a;
    ++a;
    return prev;
}

BigInteger operator--(BigInteger& a, int) {
    BigInteger prev = a;
    --a;
    return prev;
}

std::weak_ordering operator<=>(const BigInteger& a, const BigInteger& b) {
    if (a.negative == b.negative) {
        if (a.digits.size() == b.digits.size()) {
            if (a.negative) {
                return BigInteger::lexicographical_compare_three_way(b.digits.rbegin(),
                                                                     b.digits.rend(),
                                                                     a.digits.rbegin(),
                                                                     a.digits.rend());
            } else {
                return BigInteger::lexicographical_compare_three_way(a.digits.rbegin(),
                                                                     a.digits.rend(),
                                                                     b.digits.rbegin(),
                                                                     b.digits.rend());
            }
        } else {
            if (a.negative) {
                return b.digits.size() <=> a.digits.size();
            } else {
                return a.digits.size() <=> b.digits.size();
            }
        }
    } else {
        return b.negative <=> a.negative;
    }
}

bool operator==(const BigInteger& a, const BigInteger& b) = default;

bool operator!=(const BigInteger& a, const BigInteger& b) = default;

BigInteger operator "" _bi(const char* s) {
    return BigInteger(std::string(s));
}

std::ostream& operator<<(std::ostream& out, const BigInteger& bi) {
    return out << bi.toString();
}

std::istream& operator>>(std::istream& in, BigInteger& bi) {
    int ch = in.get();
    while (ch == ' ' || ch == '\n' || ((ch < '0' || ch > '9') && ch != '-')) {
        ch = in.get();
    }
    std::string s;
    while (ch != ' ' && ch != '\n' && ((ch >= '0' && ch <= '9') || ch == '-') && !in.eof()) {
        s.push_back(static_cast<char>(ch));
        ch = in.get();
    }
    bi = BigInteger(s);
    return in;
}

BigInteger operator+(BigInteger a, const BigInteger& b) {
    return a += b;
}

BigInteger operator-(BigInteger a, const BigInteger& b) {
    return a -= b;
}

BigInteger operator*(BigInteger a, const BigInteger& b) {
    return a *= b;
}

BigInteger operator/(BigInteger a, const BigInteger& b) {
    return a /= b;
}

BigInteger operator%(BigInteger a, const BigInteger& b) {
    return a %= b;
}

BigInteger gcd(const BigInteger& a, const BigInteger& b) {
    if (b == 0) {
        return a;
    } else {
        return gcd(b, a % b);
    }
}

class Rational {
    BigInteger num;
    BigInteger denom;
 public:
    Rational(const BigInteger& bi) : num(bi), denom(1) {

    }
    Rational(int i) : Rational(BigInteger(i)) {

    }
    Rational(const BigInteger& num, const BigInteger& denom) : num(num), denom(denom) {
        simplify();
    }
    Rational() : num(0), denom(1) {

    }
    Rational& operator*=(const Rational& other) {
        num *= other.num;
        denom *= other.denom;
        simplify();
        return *this;
    }

    Rational& operator/=(const Rational& other) {
        num *= other.denom;
        denom *= other.num;
        simplify();
        return *this;
    }

    Rational& operator+=(const Rational& other) {
        BigInteger g = gcd(denom, other.denom);
        num *= (other.denom / g);
        num += other.num * (denom / g);
        denom *= (other.denom / g);
        simplify();
        return *this;
    }

    Rational operator-() const {
        return {-num, denom};
    }

    Rational& operator-=(const Rational& other) {
        return *this += (-other);
    }

    std::string toString() const {
        if (denom == 1) {
            return num.toString();
        } else {
            return num.toString() + "/" + denom.toString();
        }
    }

    std::string asDecimal(size_t precision = 0) const {
        BigInteger num2 = num;
        for (size_t i = 0; i < precision; ++i) {
            num2 *= 10;
        }
        BigInteger res = num2 / denom;
        bool neg = res < 0;
        if (neg) {
            res *= -1;
        }
        std::string s = res.toString();
        if (s.length() <= precision) {
            s = std::string(precision - s.length() + 1, '0') + s;
        }
        s.insert(s.end() - static_cast<long>(precision), '.');
        if (neg) {
            s = "-" + s;
        }
        return s;
    }

    explicit operator double() const {
        BigInteger bi = num;
        for (int i = 0; i < 53; ++i) {
            bi *= 2;
        }
        BigInteger res = bi / denom;
        return res.toDouble() / static_cast<double>(1ll << 53);
    }

    friend std::weak_ordering operator<=>(const Rational& a, const Rational& b);

    friend bool operator==(const Rational& a, const Rational& b);

    friend bool operator!=(const Rational& a, const Rational& b);

 private:
    void simplify() {
        BigInteger g = gcd(num, denom);
        num /= g;
        denom /= g;
        if (denom < 0) {
            num *= -1;
            denom *= -1;
        }
    }
};

Rational operator*(Rational a, const Rational& b) {
    return a *= b;
}

Rational operator/(Rational a, const Rational& b) {
    return a /= b;
}

Rational operator+(Rational a, const Rational& b) {
    return a += b;
}

Rational operator-(Rational a, const Rational& b) {
    return a -= b;
}

std::weak_ordering operator<=>(const Rational& a, const Rational& b) {
    return (a.num * b.denom) <=> (b.num * a.denom);
}

bool operator==(const Rational& a, const Rational& b) = default;

bool operator!=(const Rational& a, const Rational& b) = default;
