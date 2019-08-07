# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, List, Optional, Sequence, Union

from libcst._maybe_sentinel import MaybeSentinel
from libcst._nodes._expression import Annotation, Name, Param, Parameters, ParamStar
from libcst._nodes._op import AssignEqual, Comma
from libcst._parser._custom_itertools import grouper
from libcst._parser._production_decorator import with_production
from libcst._parser._types.config import ParserConfig
from libcst._parser._types.partials import ParamStarPartial
from libcst._parser._whitespace_parser import parse_parenthesizable_whitespace


@with_production(  # noqa: C901: too complex
    "typedargslist",
    (
        "(tfpdef_assign (',' tfpdef_assign)* "
        + "[',' [tfpdef_star (',' tfpdef_assign)* [',' [tfpdef_starstar [',']]] | tfpdef_starstar [',']]]"
        + "| tfpdef_star (',' tfpdef_assign)* [',' [tfpdef_starstar [',']]] | tfpdef_starstar [','])"
    ),
)
@with_production(
    "varargslist",
    (
        "(vfpdef_assign (',' vfpdef_assign)* "
        + "[',' [vfpdef_star (',' vfpdef_assign)* [',' [vfpdef_starstar [',']]] | vfpdef_starstar [',']]]"
        + "| vfpdef_star (',' vfpdef_assign)* [',' [vfpdef_starstar [',']]] | vfpdef_starstar [','])"
    ),
)
def convert_argslist(config: ParserConfig, children: Sequence[Any]) -> Any:
    params: List[Param] = []
    default_params: List[Param] = []
    star_arg: Union[Param, ParamStar, MaybeSentinel] = MaybeSentinel.DEFAULT
    kwonly_params: List[Param] = []
    star_kwarg: Optional[Param] = None

    def add_param(
        current_param: Optional[List[Param]], param: Union[Param, ParamStar]
    ) -> Optional[List[Param]]:
        nonlocal star_arg
        nonlocal star_kwarg

        if isinstance(param, ParamStar):
            # Only can add this if we don't already have a "*" or a "*param".
            if current_param in [params, default_params]:
                star_arg = param
                current_param = kwonly_params
            else:
                # TODO: We need to inform the user of an invalid syntax here
                raise Exception("Syntax error!")
        elif isinstance(param.star, str) and param.star == "" and param.default is None:
            # Can only add this if we're in the params or kwonly_params section
            if current_param is params:
                params.append(param)
            elif current_param is kwonly_params:
                kwonly_params.append(param)
            else:
                # TODO: We need to inform the user of an invalid syntax here
                raise Exception("Syntax error!")
        elif (
            isinstance(param.star, str)
            and param.star == ""
            and param.default is not None
        ):
            if current_param is params:
                current_param = default_params
            # Can only add this if we're not yet at star args.
            if current_param is default_params:
                default_params.append(param)
            elif current_param is kwonly_params:
                kwonly_params.append(param)
            else:
                # TODO: We need to inform the user of an invalid syntax here
                raise Exception("Syntax error!")
        elif (
            isinstance(param.star, str) and param.star == "*" and param.default is None
        ):
            # Can only add this if we're in params/default_params, since
            # we only allow one of "*" or "*param".
            if current_param in [params, default_params]:
                star_arg = param
                current_param = kwonly_params
            else:
                # TODO: We need to inform the user of an invalid syntax here
                raise Exception("Syntax error!")
        elif (
            isinstance(param.star, str) and param.star == "**" and param.default is None
        ):
            # Can add this in all cases where we don't have a star_kwarg
            # yet.
            if current_param is not None:
                star_kwarg = param
                current_param = None
            else:
                # TODO: We need to inform the user of an invalid syntax here
                raise Exception("Syntax error!")
        else:
            # TODO: We need to inform the user of an invalid syntax here
            raise Exception("Syntax error!")

        return current_param

    # The parameter list we are adding to
    current: Optional[List[Param]] = params

    # We should have every other item in the group as a param or a comma by now,
    # so split them up, add commas and then put them in the appropriate group.
    for parameter, comma in grouper(children, 2):
        if comma is None:
            if isinstance(parameter, ParamStarPartial):
                # TODO: We need to inform the user of an invalid syntax here
                raise Exception("Syntax error!")
            else:
                current = add_param(current, parameter)
        else:
            comma = Comma(
                whitespace_before=parse_parenthesizable_whitespace(
                    config, comma.whitespace_before
                ),
                whitespace_after=parse_parenthesizable_whitespace(
                    config, comma.whitespace_after
                ),
            )
            if isinstance(parameter, ParamStarPartial):
                current = add_param(current, ParamStar(comma=comma))
            else:
                current = add_param(current, parameter.with_changes(comma=comma))

    return Parameters(
        params=tuple(params),
        default_params=tuple(default_params),
        star_arg=star_arg,
        kwonly_params=tuple(kwonly_params),
        star_kwarg=star_kwarg,
    )


@with_production("tfpdef_star", "'*' [tfpdef]")
@with_production("vfpdef_star", "'*' [vfpdef]")
def convert_fpdef_star(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (star,) = children
        return ParamStarPartial()
    else:
        star, param = children
        return param.with_changes(
            star=star.string,
            whitespace_after_star=parse_parenthesizable_whitespace(
                config, star.whitespace_after
            ),
        )


@with_production("tfpdef_starstar", "'**' tfpdef")
@with_production("vfpdef_starstar", "'**' vfpdef")
def convert_fpdef_starstar(config: ParserConfig, children: Sequence[Any]) -> Any:
    starstar, param = children
    return param.with_changes(
        star=starstar.string,
        whitespace_after_star=parse_parenthesizable_whitespace(
            config, starstar.whitespace_after
        ),
    )


@with_production("tfpdef_assign", "tfpdef ['=' test]")
@with_production("vfpdef_assign", "vfpdef ['=' test]")
def convert_fpdef_assign(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        (child,) = children
        return child

    param, equal, default = children
    return param.with_changes(
        equal=AssignEqual(
            whitespace_before=parse_parenthesizable_whitespace(
                config, equal.whitespace_before
            ),
            whitespace_after=parse_parenthesizable_whitespace(
                config, equal.whitespace_after
            ),
        ),
        default=default.value,
    )


@with_production("tfpdef", "NAME [':' test]")
@with_production("vfpdef", "NAME")
def convert_fpdef(config: ParserConfig, children: Sequence[Any]) -> Any:
    if len(children) == 1:
        # This is just a parameter
        (child,) = children
        namenode = Name(child.string)
        annotation = None
    else:
        # This is a parameter with a type hint
        name, colon, typehint = children
        namenode = Name(name.string)
        annotation = Annotation(
            whitespace_before_indicator=parse_parenthesizable_whitespace(
                config, colon.whitespace_before
            ),
            whitespace_after_indicator=parse_parenthesizable_whitespace(
                config, colon.whitespace_after
            ),
            annotation=typehint.value,
        )

    return Param(star="", name=namenode, annotation=annotation, default=None)
