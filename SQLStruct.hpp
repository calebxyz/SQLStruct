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
#include <array>
#include <concepts>

using namespace std::literals;

struct empty{};

template <typename Specialization, template <typename...> typename Basetype>
concept specialization_of_types_only = requires (Specialization s) { 
    []()-> decltype(Basetype(s)) {}.operator()();
};


//evil and ugly code , maybe i will find a better solution in the future 
#define create_specialization_concept(Name, Basetype)                          \
template <typename Specialization>                                             \
concept specialization_##Name = requires (Specialization s){                   \
    []()-> decltype(Basetype(s)) {}.operator()();                              \
}

#define create_specialization_type_name(Basetype) create_specialization_concept(Basetype, Basetype)


template <std::integral Integral>
    static constexpr std::size_t get_integral_size(Integral i){
        std::size_t count{0};
        Integral num = i;
        while (num > 0) {
            count++;
            num /= 10;
        }

        return count; 
    }

template <std::size_t Size>
struct fixed_string {
    char _data[Size + 1]{0};
    static constexpr std::size_t _size = Size;

    constexpr explicit(false) fixed_string(char const* str) {
        std::copy_n(str, Size + 1, _data);
    }

    template <std::integral Integral> 
    constexpr explicit(false) fixed_string(Integral i){
        number_to_string(i, get_integral_size(i));
    }

    constexpr explicit(false) fixed_string() {
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

    private: 
    
    //constexpr algorithm ro turn numbers to strings 
    // in c++23 we will be able to use to_chars
    template <std::integral Integral>
    constexpr void number_to_string(Integral i, std::size_t count){
        Integral num = i; 

        while (count > 0){
            count--;
            char c = '0' + num % 10; 
            _data[count] = c; 
            num /= 10;
        }
    }
};

create_specialization_type_name(fixed_string);


//helper for empty fixed_string
fixed_string() -> fixed_string<0>;

template <unsigned int Size> 
fixed_string(char const (&)[Size]) -> fixed_string<Size - 1>;

template <std::integral Integral> 
fixed_string(Integral) -> fixed_string<255>;

template <auto i> requires std::integral<decltype(i)>
constexpr auto make_integral_fixed_string() {
    return fixed_string<get_integral_size(i)>(i);
}

template<auto Key, typename Val>
struct alignas(alignof(int)) schema_field {
    using ArgType = Val;
    static constexpr auto _key = Key;
    using Pure_Key_Type = std::decay_t<decltype(Key)>; 
    Val _val{};
     

    template<typename T>
    constexpr auto operator=(const T& val) const {
        return schema_field<Key, T>{._val = val};
    }

    constexpr explicit(false) operator std::string_view() const {
        return _strKey;
    }

    private:
    static constexpr auto get_str_key() requires specialization_fixed_string<Pure_Key_Type> and 
    (not std::integral<Pure_Key_Type>){
        return Key;
    }

    static constexpr auto get_str_key() requires std::integral<Pure_Key_Type>{
        return make_integral_fixed_string<Key>();
    }

    static constexpr auto get_str_key() requires std::convertible_to<Pure_Key_Type, std::string_view> and 
    (not std::integral<Pure_Key_Type>) and (not specialization_fixed_string<Pure_Key_Type>){
        return fixed_string<Key.size()>(Key.data());
    }
    
    static constexpr auto _strKey = get_str_key();
    static constexpr std::size_t _size = _strKey.size();
};


create_specialization_type_name(schema_field);

template<fixed_string Name>
constexpr auto operator""_a() { return schema_field<Name, empty>{}; };

template<fixed_string Name>
constexpr auto operator""_fs() { return Name; };

template<std::integral_constant idx>
    constexpr auto operator""_ia() { return schema_field<idx, schema_field<""_fs, empty>>{}; };

template<specialization_schema_field... schema_fields> 
struct schema : public schema_fields...{}; 

namespace impl{
    //if we cant get the type use this function to return the default type 
    template<typename Default, auto Key, template <auto, typename> typename>
    auto find_sliced_type(...) -> Default;

    //if we can deduce the argument type then this function will return the correct argument type that was inhereted from 
    // this works due to the fact that sqlstruct inherits all arguments 
    template<typename, auto Key, template <auto, typename> typename Arg, typename Val>
    requires (std::integral<decltype(Key)> || std::convertible_to<decltype(Key), std::string_view>)
    auto find_sliced_type(Arg<Key, Val>*) -> Arg<Key, Val>;  
}

template<typename MultiStruct, typename Default, auto Key, template <auto, typename> typename Arg>
using find_sliced_type = decltype(impl::find_sliced_type<Default, Key, Arg>(static_cast<MultiStruct*>(nullptr)));

template <specialization_schema_field... schema_fields> 
struct SQLStruct : public schema_fields... {
    template<std::size_t... Ns>
    static constexpr decltype(auto) get_type_for_ind(std::index_sequence<Ns...>){
        return schema<
        schema_field<static_cast<std::size_t>(Ns), schema_fields>...>{};
    }

    constexpr explicit(true) SQLStruct(schema_fields... fields): schema_fields{fields}...
    {};

    //this is just amazing we are passing non template parameters and the deduction rules 
    //just know how to complete them!!!!
    template <typename T, typename ARG = find_sliced_type<SQLStruct, empty, T::_key, schema_field>>
    requires (!std::same_as<empty, ARG>)
    constexpr const auto& operator[](const T) const {
        return static_cast<const ARG*>(this)->_val;
    }

    template <typename T, typename ARG = find_sliced_type<SQLStruct, empty, T::_key, schema_field>>
    requires (!std::same_as<empty, ARG>)
    constexpr auto& operator[](const T) {
        return static_cast<ARG*>(this)->_val;
    }

    template <std::size_t N, 
    typename ArgMap = std::decay_t<decltype(get_type_for_ind(std::make_index_sequence<sizeof...(schema_fields)>{}))>,
    typename Arg = find_sliced_type<ArgMap, empty, N, schema_field>>
    auto& get() requires (!std::same_as<empty, Arg>) {
        return *static_cast<typename Arg::ArgType*>(this);
    }

    template <std::size_t N, 
    typename ArgMap = std::decay_t<decltype(get_type_for_ind(std::make_index_sequence<sizeof...(schema_fields)>{}))>,
    typename Arg = find_sliced_type<ArgMap, empty, N, schema_field>>
    const auto& get() const requires (!std::same_as<empty, Arg>) {
        return *static_cast<typename Arg::ArgType*>(this);
    }

    static constexpr std::size_t size = sizeof...(schema_fields);
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
        using type = typename find_sliced_type<ArgMap, ::empty, IDX, schema_field>::ArgType;
        static_assert(!is_same_v<type, ::empty>);
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
