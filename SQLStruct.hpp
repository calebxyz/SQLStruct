#include <memory> 
#include <vector> 
#include <type_traits>
#include <map>
#include <string>
#include <iostream>
#include <variant>
#include <cstdint>
#include <cmath>
#include <math.h>
#include <algorithm>
#include <any>
#include <charconv>
#include <utility>

using namespace std::literals;

struct void_t{};

namespace impl{
template <auto f>
struct is_specialization_of {
private:
    template <class T>
    static auto get_val(int) -> std::is_same<T, decltype(f.template operator()<T>())>;

    template <class T>
    static auto get_val(...) -> std::false_type;

public:
    template <class T>
    static constexpr bool value = decltype(get_val<T>(0))::value;
};
}

#define is_specialization_of(TemplateType) \
    impl::is_specialization_of<[]<class T>() -> decltype(TemplateType(std::declval<T>())) { }>::template value

template <std::size_t Size>
struct fixed_string {
    char _data[Size + 1]{0};
    static constexpr std::size_t _size = Size;

    constexpr explicit(false) fixed_string(char const* str) {
        std::copy_n(str, Size + 1, _data);
    }

    //constexpr algorithm ro turn numbers to strings 
    template <typename Integral> requires (std::is_integral_v<Integral>) 
    constexpr explicit(false) fixed_string(Integral i){
        auto num{i};
        auto count{0};  
        while (num > 0) {
            count++;
            num /= 10;
        }

        num = i; 

        while (count > 0){
            count--;
            char c = '0' + num % 10; 
            _data[count] = c; 
            num /= 10;
        }
    }

    constexpr explicit(false) operator std::string_view() const {
        return {_data, _size};
    }

    constexpr std::size_t size() const {
        return _size;
    }

    constexpr const char* data() const {
        return _data;
    }

    constexpr auto operator<=>(const fixed_string&) const = default;
};

template <unsigned int Size> 
fixed_string(char const (&)[Size]) -> fixed_string<Size - 1>;

template <typename T> 
fixed_string(T) -> fixed_string<254>;


template<auto Key, typename Val>
struct alignas(alignof(int)) Argument {
    using ArgType = Val;
    static constexpr auto _key = Key; 
    Val _val{};
     

    template<typename T>
    constexpr auto operator=(const T& val) const {
        return Argument<Key, T>{._val = val};
    }

    constexpr explicit(false) operator std::string_view() const {
        return _strKey;
    }

    private:
    static constexpr auto get_str_key(){
        if constexpr (std::is_convertible_v<decltype(Key), std::string_view>){
            return fixed_string<Key.size()>(Key.data());
        }

        return fixed_string(Key);
    } 
    
    static constexpr auto _strKey = get_str_key();
    static constexpr std::size_t _size = _strKey.size();
};


template<fixed_string Name>
constexpr auto operator""_a() { return Argument<Name, void_t>{}; };

template<fixed_string Name>
constexpr auto operator""_fs() { return Name; };

template<std::integral_constant idx>
    constexpr auto operator""_ia() { return Argument<idx, Argument<""_fs, void_t>>{}; };

template<typename... T>
struct mult_inherit_composition : public T...{}; 

namespace impl{
    //if we cant get the type use this function to return the default type 
    template<typename Default, auto Key, template <auto, typename> typename>
    auto find_sliced_type(...) -> Default;

    //if we can deduce the argument tyype then this function will return the correct argument type that was inhereted from 
    // this works due to the fact that sqlstruct inherits all arguments 
    template<typename, auto Key, template <auto, typename> typename Arg, typename Val>
    auto find_sliced_type(Arg<Key, Val>*) -> Arg<Key, Val> requires (std::is_integral_v<decltype(Key)> || std::is_convertible_v<decltype(Key), std::string_view>);  
}

template<typename MultiStruct, typename Default, auto Key, template <auto, typename> typename Arg>
using find_sliced_type = decltype(impl::find_sliced_type<Default, Key, Arg>(static_cast<MultiStruct*>(nullptr)));

template <typename... Arguments>
struct SQLStruct : public Arguments... {
    template<std::size_t... Ns>
    static constexpr decltype(auto) get_type_for_ind(std::index_sequence<Ns...>){
        return mult_inherit_composition<
        Argument<static_cast<std::size_t>(Ns), Arguments>...>{};
    }

    constexpr explicit(true) SQLStruct(Arguments... arguments): Arguments{arguments}...
    {static_assert((is_specialization_of(Argument)<Arguments> && ...));};

    //this is just amazing we are passing non template parameters and the deduction rules 
    //just know how to complete them!!!!
    template <typename T, typename ARG = find_sliced_type<SQLStruct, void_t, T::_key, Argument>>
    constexpr const auto& operator[](const T) const requires (!std::is_same_v<void_t, ARG>) {
        return static_cast<const ARG*>(this)->_val;
    }

    template <typename T, typename ARG = find_sliced_type<SQLStruct, void_t, T::_key, Argument>>
    constexpr auto& operator[](const T) requires (!std::is_same_v<void_t, ARG>){
        return static_cast<ARG*>(this)->_val;
    }

    template <std::size_t N, 
    auto ArgMap = get_type_for_ind(std::make_index_sequence<sizeof...(Arguments)>{}), 
    typename Arg = find_sliced_type<decltype(ArgMap), void_t, N, Argument>>
    auto& get() requires (!std::is_same_v<void_t, Arg>) {
        return *static_cast<typename Arg::ArgType*>(this);
    }

    template <std::size_t N, 
    auto ArgMap = get_type_for_ind(std::make_index_sequence<sizeof...(Arguments)>{}), 
    typename Arg = find_sliced_type<decltype(ArgMap), void_t, N, Argument>>
    const auto& get() const requires (!std::is_same_v<void_t, Arg>) {
        return *static_cast<typename Arg::ArgType*>(this);
    }

    static constexpr std::size_t size = sizeof...(Arguments);
};


template <typename... Arguments>
SQLStruct(Arguments...) -> SQLStruct<Arguments...>; 

namespace std{

    template<typename... Args>
    struct tuple_size<SQLStruct<Args...>>{
        static constexpr size_t value = SQLStruct<Args...>::size;
    };

    template <size_t IDX, typename... Arguments>
    struct tuple_element<IDX, SQLStruct<Arguments...>>{
        using SQL = SQLStruct<Arguments...>;
        using ArgMap = decltype(SQL::get_type_for_ind(std::make_index_sequence<sizeof...(Arguments)>{}));
        using type = typename find_sliced_type<ArgMap, void_t<>, IDX, Argument>::ArgType;
        static_assert(!is_same_v<type, void_t<>>);
    };

    template <size_t IDX, typename SQLStruct>
    auto& get(SQLStruct& sql){
        return sql.template get<IDX>();
    } 

    template <size_t IDX, typename SQLStruct>
    const auto& get(const SQLStruct& sql){
        return sql.template get<IDX>();
    } 
}


int main(){
    static_assert(is_specialization_of(std::array)<std::array<int, 4>>);
    static_assert(is_specialization_of(std::tuple)<std::tuple<int, double, char>>);
    static_assert(is_specialization_of(Argument)<Argument<10, int>>);
    constexpr auto fs = "Itai"_fs;
    static_assert(fs == "Itai"sv);
    static_assert("Ari"sv == "Ari"_a);
    auto arg1 = ("Ari"_a = 10);
    constexpr auto sql1 = SQLStruct("x"_a = 10, "y"_a = 20.05f);
    static_assert(10 == sql1["x"_a]);
    auto sql2 = SQLStruct("x"_a = 10, "y"_a = 20.05f, "Itay"_a = std::array<int, 3>{{1, 2, 3}});
    sql2["x"_a] = 11;
    sql2["y"_a] = 20.11f;
    sql2["Itay"_a][2] = 5; 
    auto arg3 = std::get<0>(sql2);
    auto& [a1, a2, a3] = sql2;
    return arg3._val + a1._val;
}
